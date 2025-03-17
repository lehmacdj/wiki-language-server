-- | Probably want to add my own `getLogger` function that I can override with
-- local eventually so I can add context to the logging that I do here
module Utils.Logging where

import Colog.Core hiding (HasLog (..))
import LSP.Raw
import Language.LSP.Logging
import MyPrelude

data Logging :: Effect where
  LogMessage :: WithSeverity Text -> Logging m ()

makeEffect ''Logging

runLoggingLSP :: (LSP :> es, IOE :> es) => Eff (Logging : es) a -> Eff es a
runLoggingLSP = interpret_ \case
  LogMessage msg -> defaultClientLogger <& msg

defaultLogger :: (Logging :> es) => LogAction (Eff es) (WithSeverity Text)
defaultLogger = LogAction logMessage

logWarn :: (Logging :> es) => Text -> Eff es ()
logWarn msg = defaultLogger <& WithSeverity msg Warning

logError :: (Logging :> es) => Text -> Eff es ()
logError msg = defaultLogger <& WithSeverity msg Error

logException :: (Logging :> es, Exception e) => e -> Eff es ()
logException e = defaultLogger <& WithSeverity (tshow e) Error

logInfo :: (Logging :> es) => Text -> Eff es ()
logInfo msg = defaultLogger <& WithSeverity msg Info

logDebug :: (Logging :> es) => Text -> Eff es ()
logDebug msg = defaultLogger <& WithSeverity msg Debug
