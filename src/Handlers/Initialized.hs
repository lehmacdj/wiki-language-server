module Handlers.Initialized where

import Handlers.Prelude
import Models.WikiLanguageServerConfig
import MyPrelude

initialized ::
  (MonadLsp Config m) => TNotificationMessage 'Method_Initialized -> m ()
initialized _n = pure ()
