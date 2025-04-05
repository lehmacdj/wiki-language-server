module Handlers.Initialized where

import Handlers.Prelude
import MyPrelude

-- | This handler is necessary to avoid logging a warning to clients:
-- @LSP: No handler for 'initialized'@
initialized ::
  (MonadLsp Config m) => TNotificationMessage 'Method_Initialized -> m ()
initialized _n = pure ()
