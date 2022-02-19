module Wiki.LSP.Config where

import Data.Aeson
import MyPrelude

newtype Config = Config
  { maxDiagnostics :: Int
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Default Config where
  def =
    Config
      { maxDiagnostics = 100
      }

-- TODO: does this need patch like semantics
onConfigChange :: Config -> Value -> Either Text Config
onConfigChange _old v = case fromJSON v of
  Error e -> Left $ pack e
  Success new -> Right new
