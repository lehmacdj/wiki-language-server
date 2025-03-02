module Wiki.LSP.Config where

import Data.Aeson
import MyPrelude

-- | This is a config value that can be passed to the server from the client on
-- initialization.
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
parseConfig :: Config -> Value -> Either Text Config
parseConfig _old v = case fromJSON v of
  Error e -> Left $ pack e
  Success new -> Right new
