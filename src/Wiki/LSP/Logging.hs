-- | Probably want to add my own `getLogger` function that I can override with
-- local eventually so I can add context to the logging that I do here
module Wiki.LSP.Logging where

import Colog.Core
import Language.LSP.Logging
import Language.LSP.Server
import MyPrelude

logWarn :: (MonadLsp c m) => Text -> m ()
logWarn msg = defaultClientLogger <& WithSeverity msg Warning

logError :: (MonadLsp c m) => Text -> m ()
logError msg = defaultClientLogger <& WithSeverity msg Error

logException :: (MonadLsp c m, Exception e) => e -> m ()
logException e = defaultClientLogger <& WithSeverity (tshow e) Error

logInfo :: (MonadLsp c m) => Text -> m ()
logInfo msg = defaultClientLogger <& WithSeverity msg Info

logDebug :: (MonadLsp c m) => Text -> m ()
logDebug msg = defaultClientLogger <& WithSeverity msg Debug
