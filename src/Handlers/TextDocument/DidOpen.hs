module Handlers.TextDocument.DidOpen where

import Handlers.Prelude
import Language.LSP.Protocol.Lens as J hiding (to)
import Models.Page.Parser qualified as Page
import Models.WikiLanguageServerConfig
import MyPrelude
import Utils.LSP

textDocumentDidOpen ::
  (MonadLsp Config m) => TNotificationMessage 'Method_TextDocumentDidOpen -> m ()
textDocumentDidOpen notification = do
  let doc = notification ^. J.params . J.textDocument
      nuri = doc ^. J.uri . to toNormalizedUri
      version = doc ^. J.version . to Just
      contents = doc ^. J.text
  case Page.parse (fromMaybe "<unknown>" $ nuriToFilePath nuri) contents of
    Left d ->
      sendDiagnostics nuri version [d]
    Right _ -> pure ()
