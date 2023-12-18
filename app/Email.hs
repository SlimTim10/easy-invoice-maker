module Email where

import qualified Data.Yaml as Yaml
import qualified GHC.Generics as Generic
import qualified Data.Text as T

data Login = Login
  { server :: T.Text
  , username :: T.Text
  , password :: T.Text
  } deriving (Show, Eq, Generic.Generic, Yaml.ToJSON, Yaml.FromJSON)

data From = From
  { name :: T.Text
  , address :: T.Text
  } deriving (Show, Eq, Generic.Generic, Yaml.ToJSON, Yaml.FromJSON)

data Email = Email
  { login :: Login
  , from :: From
  , subject :: T.Text
  , footer :: T.Text
  } deriving (Show, Eq, Generic.Generic, Yaml.ToJSON, Yaml.FromJSON)

data Template = Template
  { templateToName :: T.Text
  , templateInvoiceNumber :: T.Text
  , templateFooter :: T.Text
  } deriving (Show, Eq, Generic.Generic, Yaml.ToJSON)
