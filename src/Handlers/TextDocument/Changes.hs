module Handlers.TextDocument.Changes where

import Handlers.Prelude
import LSP.Diagnostics
import MyPrelude

textDocumentDidOpen ::
  (LSP :> es, VFSAccess :> es, Diagnostics :> es) =>
  TNotificationMessage 'Method_TextDocumentDidOpen -> Eff es ()
textDocumentDidOpen notification = withEarlyReturn do
  let uri = uriFromMessage notification
  mVersionContents <- tryGetVfsUriContents uri
  (version, contents) <- onNothing mVersionContents $ returnEarly ()
  case parseDocument uri contents of
    Left d -> sendDiagnostics uri (Just version) [d]
    Right _ -> pure ()

textDocumentDidChange ::
  (LSP :> es, VFSAccess :> es, Diagnostics :> es) =>
  TNotificationMessage 'Method_TextDocumentDidChange -> Eff es ()
textDocumentDidChange notification = withEarlyReturn do
  let uri = uriFromMessage notification
  mVersionContents <- tryGetVfsUriContents uri
  (version, contents) <- onNothing mVersionContents $ returnEarly ()
  case parseDocument uri contents of
    Left d -> sendDiagnostics uri (Just version) [d]
    Right _ -> pure ()
