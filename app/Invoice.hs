module Invoice where

import qualified Data.Yaml as Yaml
import Data.Yaml ((.:), (.:?), (.=))
import qualified GHC.Generics as Generic
import qualified Money
import qualified Data.Text as T

data From = From
  { fromName :: T.Text
  , fromWebsite :: Maybe T.Text
  , fromEmail :: T.Text
  } deriving (Show, Eq, Generic.Generic, Yaml.ToJSON)

instance Yaml.FromJSON From where
  parseJSON = Yaml.withObject "InvoiceFrom" $ \o -> From
    <$> o .: "name"
    <*> o .:? "website"
    <*> o .: "email"

data To = To
  { toName :: T.Text
  , toEmail :: T.Text
  } deriving (Show, Eq, Generic.Generic, Yaml.ToJSON)

instance Yaml.FromJSON To where
  parseJSON = Yaml.withObject "InvoiceTo" $ \o -> To
    <$> o .: "name"
    <*> o .: "email"

data Item = Item
  { itemName :: T.Text
  , itemDescription :: Maybe T.Text
  , itemQtyHrs :: Float
  , itemUnitPrice :: Money.Dense "CAD"
  , itemTotalPrice :: Money.Dense "CAD"
  } deriving (Show, Eq, Generic.Generic)

instance Yaml.ToJSON Item where
  toJSON (Item nm des qh up tp) = Yaml.object
    [ ("itemName" .= nm)
    , ("itemDescription" .= des)
    , ("itemQtyHrs" .= qh)
    , ("itemUnitPrice" .= showMoney up)
    , ("itemTotalPrice" .= showMoney tp)
    ]

instance Yaml.FromJSON Item where
  parseJSON = Yaml.withObject "item" $ \o -> do
    name <- o .: "name"
    description <- o .:? "description"
    qtyHrs <- o .: "qtyHrs"
    unitPriceRaw <- o .: "unitPrice"
    let unitPrice = floatToMoney unitPriceRaw
    let totalPrice = floatToMoney qtyHrs * unitPrice
    return $ Item name description qtyHrs unitPrice totalPrice

data InvoiceDefaults = InvoiceDefaults
  { defaultFrom :: Maybe From
  , defaultNotes :: Maybe T.Text
  } deriving (Show, Eq, Generic.Generic, Yaml.ToJSON)

instance Yaml.FromJSON InvoiceDefaults where
  parseJSON = Yaml.withObject "InvoiceDefaults" $ \o -> InvoiceDefaults
    <$> o .:? "from"
    <*> o .:? "notes"

data InvoiceBody = InvoiceBody
  { bodyDate :: Maybe T.Text
  , bodyFrom :: Maybe From
  , bodyTo :: To
  , bodyItems :: [Item]
  } deriving (Show, Eq, Generic.Generic, Yaml.ToJSON)

instance Yaml.FromJSON InvoiceBody where
  parseJSON = Yaml.withObject "InvoiceBody" $ \o -> InvoiceBody
    <$> o .:? "date"
    <*> o .:? "from"
    <*> o .: "to"
    <*> o .: "items"

data Invoice = Invoice
  { invNumber :: T.Text
  , invDate :: T.Text
  , invFrom :: From
  , invTo :: To
  , invItems :: [Item]
  , invBalanceDue :: Money.Dense "CAD"
  , invNotes :: Maybe T.Text
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
