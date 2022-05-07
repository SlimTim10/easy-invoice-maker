module Main where

import qualified Data.Time.Clock as Clock
import qualified Data.Time.Calendar as Cal
import qualified Data.Decimal as Decimal

data InvoiceFrom = InvoiceFrom
  { fromName :: String
  , fromWebsite :: String
  , fromEmail :: String
  }

data InvoiceTo = InvoiceTo
  { toName :: String
  , toEmail :: String
  }

data InvoiceItem = InvoiceItem
  { itemTitle :: String
  , itemDescription :: String
  , itemQtyHrs :: Decimal.Decimal
  , itemUnitPrice :: Decimal.Decimal
  , itemTotal :: Decimal.Decimal
  }

data Invoice = Invoice
  { invNumber :: Integer
  , invDate :: Integer
  , invFrom :: InvoiceFrom
  , invTo :: InvoiceTo
  , invItems :: [InvoiceItem]
  , invNotes :: String
  }

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  date >>= print

-- (year, month, day)
date :: IO (Integer, Int, Int)
date = Clock.getCurrentTime >>= return . Cal.toGregorian . Clock.utctDay
