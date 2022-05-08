{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
module Main where

import Data.Maybe (fromMaybe)
import qualified Data.Text.IO as TIO
import qualified Data.Time.Clock as Time
import qualified Data.Time.Format as Time
-- import qualified Data.Decimal as Decimal
import qualified Text.Mustache as Mustache
import qualified Data.Yaml as Yaml
import Data.Yaml ((.:), (.:?), (.=))
import qualified GHC.Generics as Generic
import qualified Data.Text as T
import qualified Money
import qualified System.Environment as Env
import qualified System.FilePath as FP

data InvoiceFrom = InvoiceFrom
  { fromName :: String
  , fromWebsite :: Maybe String
  , fromEmail :: String
  } deriving (Show, Eq, Generic.Generic, Yaml.ToJSON)

instance Yaml.FromJSON InvoiceFrom where
  parseJSON = Yaml.withObject "InvoiceFrom" $ \o -> InvoiceFrom
    <$> o .: "name"
    <*> o .:? "website"
    <*> o .: "email"

data InvoiceTo = InvoiceTo
  { toName :: String
  , toEmail :: String
  } deriving (Show, Eq, Generic.Generic, Yaml.ToJSON)

instance Yaml.FromJSON InvoiceTo where
  parseJSON = Yaml.withObject "InvoiceTo" $ \o -> InvoiceTo
    <$> o .: "name"
    <*> o .: "email"

data InvoiceItem = InvoiceItem
  { itemName :: String
  , itemDescription :: Maybe String
  , itemQtyHrs :: Float
  , itemUnitPrice :: Money.Dense "CAD"
  , itemTotalPrice :: Money.Dense "CAD"
  } deriving (Show, Eq, Generic.Generic)

instance Yaml.ToJSON InvoiceItem where
  toJSON (InvoiceItem nm des qh up tp) = Yaml.object
    [ ("itemName" .= nm)
    , ("itemDescription" .= des)
    , ("itemQtyHrs" .= qh)
    , ("itemUnitPrice" .= showMoney up)
    , ("itemTotalPrice" .= showMoney tp)
    ]

instance Yaml.FromJSON InvoiceItem where
  parseJSON = Yaml.withObject "item" $ \o -> do
    name <- o .: "name"
    description <- o .:? "description"
    qtyHrs <- o .: "qtyHrs"
    unitPriceRaw <- o .: "unitPrice"
    let unitPrice = floatToMoney unitPriceRaw
    let totalPrice = floatToMoney qtyHrs * unitPrice
    return $ InvoiceItem name description qtyHrs unitPrice totalPrice

data InvoiceDefaults = InvoiceDefaults
  { defaultFrom :: Maybe InvoiceFrom
  , defaultNotes :: Maybe String
  } deriving (Show, Eq, Generic.Generic, Yaml.ToJSON)

instance Yaml.FromJSON InvoiceDefaults where
  parseJSON = Yaml.withObject "InvoiceDefaults" $ \o -> InvoiceDefaults
    <$> o .:? "from"
    <*> o .:? "notes"

data InvoiceBody = InvoiceBody
  { bodyDate :: Maybe String
  , bodyFrom :: Maybe InvoiceFrom
  , bodyTo :: InvoiceTo
  , bodyItems :: [InvoiceItem]
  } deriving (Show, Eq, Generic.Generic, Yaml.ToJSON)

instance Yaml.FromJSON InvoiceBody where
  parseJSON = Yaml.withObject "InvoiceBody" $ \o -> InvoiceBody
    <$> o .:? "date"
    <*> o .:? "from"
    <*> o .: "to"
    <*> o .: "items"

data Invoice = Invoice
  { invNumber :: String
  , invDate :: String
  , invFrom :: InvoiceFrom
  , invTo :: InvoiceTo
  , invItems :: [InvoiceItem]
  , invBalanceDue :: Money.Dense "CAD"
  , invNotes :: Maybe String
  } deriving (Show, Eq, Generic.Generic)

instance Yaml.ToJSON Invoice where
  toJSON (Invoice n dt f t is bal nts) = Yaml.object
    [ ("invNumber" .= n)
    , ("invDate" .= dt)
    , ("invFrom" .= f)
    , ("invTo" .= t)
    , ("invItems" .= is)
    , ("invBalanceDue" .= showMoneyWithCurrency bal)
    , ("invNotes" .= nts)
    ]

showMoney :: Money.Dense "CAD" -> T.Text
showMoney x = "$" <> Money.denseToDecimal Money.defaultDecimalConf Money.Round x

showMoneyWithCurrency :: Money.Dense "CAD" -> T.Text
showMoneyWithCurrency x =
  "$"
  <> Money.denseToDecimal Money.defaultDecimalConf Money.Round x
  <> " "
  <> Money.denseCurrency x

floatToMoney :: Float -> Money.Dense "CAD"
floatToMoney = Money.dense' . toRational

date :: IO String
date = Time.getCurrentTime >>= return . Time.formatTime Time.defaultTimeLocale "%b %-d, %Y"

main :: IO ()
main = do
  invoiceFilePath <- head <$> Env.getArgs
  Mustache.automaticCompile ["."] "template.mustache" >>= \case
    Left err -> print err
    Right template -> do
      
      defaultParams :: InvoiceDefaults <- Yaml.decodeFileThrow "defaults.yaml"
      bodyParams :: InvoiceBody <- Yaml.decodeFileThrow invoiceFilePath
      let fromParams :: InvoiceFrom =
            case bodyFrom bodyParams of
              Just x -> x
              Nothing -> case defaultFrom defaultParams of
                Just x -> x
                Nothing -> InvoiceFrom "" Nothing ""
      currentDate <- date
      let invoiceNumber = FP.takeBaseName invoiceFilePath
      let invoice = Invoice
            { invNumber = invoiceNumber
            , invDate = fromMaybe currentDate (bodyDate bodyParams)
            , invFrom = fromParams
            , invTo = bodyTo bodyParams
            , invItems = bodyItems bodyParams
            , invBalanceDue = sum . map itemTotalPrice $ bodyItems bodyParams
            , invNotes = defaultNotes defaultParams
            }

      TIO.putStrLn
        . Mustache.substitute template
        . Mustache.toMustache
        . Yaml.toJSON
        $ invoice

      -- Write to a file instead
      
      -- let outFilePath = FP.replaceExtension invoiceFilePath "html"
      
      -- TIO.writeFile outFilePath
      --   . Mustache.substitute template
      --   . Mustache.toMustache
      --   . Yaml.toJSON
      --   $ invoice

      -- putStrLn $ "Wrote to file: " ++ outFilePath
