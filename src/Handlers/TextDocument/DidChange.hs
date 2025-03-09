module Handlers.TextDocument.DidChange where

import Handlers.Prelude
import Models.Diagnostics
import Models.WikiLanguageServerConfig
import MyPrelude
import Utils.LSP

textDocumentDidChange ::
  (MonadLsp Config m) => TNotificationMessage 'Method_TextDocumentDidChange -> m ()
textDocumentDidChange notification = withEarlyReturn $ do
  (nuri, mVersionContents) <- tryGetContents notification
  (version, contents) <- onNothing mVersionContents $ returnEarly ()
  case parseDocument nuri contents of
    Left d ->
      sendDiagnostics nuri (Just version) [d]
    Right _ ->
      sendDiagnostics
        nuri
        (Just version)
        [mkDiagnostic GeneralInfo (atLineCol 0 0) "didChange: Parsed successfully!"]
