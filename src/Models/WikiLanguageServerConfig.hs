module Models.WikiLanguageServerConfig where

import Data.Aeson.KeyMap qualified as KeyMap
import MyPrelude

-- | This is a config value that can be passed to the server from the client on
-- initialization.
data Config = Config
  { maxDiagnostics :: Int,
    updateNoteCacheTaskDelay :: NominalDiffTime
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Default Config where
  def =
    Config
      { maxDiagnostics = 100,
        updateNoteCacheTaskDelay = 120
      }

-- TODO: does this need patch like semantics
parseConfig :: Config -> Value -> Either Text Config
parseConfig _old Null = Right def
parseConfig _old (Object km) | KeyMap.null km = Right def
parseConfig _old v = fromJSON v
