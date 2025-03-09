module Handlers.TextDocument.Changes where

import Handlers.Prelude
import Models.WikiLanguageServerConfig
import MyPrelude
import Utils.LSP

textDocumentDidOpen ::
  (MonadLsp Config m) => TNotificationMessage 'Method_TextDocumentDidOpen -> m ()
textDocumentDidOpen notification = withEarlyReturn do
  let uri = uriFromMessage notification
  mVersionContents <- tryGetVfsUriContents uri
  (version, contents) <- onNothing mVersionContents $ returnEarly ()
  case parseDocument uri contents of
    Left d -> sendDiagnostics uri (Just version) [d]
    Right _ -> pure ()

textDocumentDidChange ::
  (MonadLsp Config m) => TNotificationMessage 'Method_TextDocumentDidChange -> m ()
textDocumentDidChange notification = withEarlyReturn do
  let uri = uriFromMessage notification
  mVersionContents <- tryGetVfsUriContents uri
  (version, contents) <- onNothing mVersionContents $ returnEarly ()
  case parseDocument uri contents of
    Left d -> sendDiagnostics uri (Just version) [d]
    Right _ -> pure ()
