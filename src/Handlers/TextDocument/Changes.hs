module Handlers.TextDocument.Changes where

import Data.IxSet.Typed qualified as IxSet
import Effectful.State.Static.Shared
import Handlers.Prelude
import Models.NoteInfo
import Models.Page.Utils qualified as Page
import Models.Slug qualified as Slug
import MyPrelude

onDocumentChange ::
  ( LSP :> es,
    VFSAccess :> es,
    Diagnostics :> es,
    State NoteInfoCache :> es
  ) =>
  NormalizedUri -> Eff es ()
onDocumentChange uri = withEarlyReturn do
  mVersionContents <- tryGetVfsUriContents uri
  (version, contents) <- onNothing mVersionContents $ returnEarly ()
  case parseDocument uri contents of
    Left d -> sendDiagnostics uri (Just version) [d]
    Right parsed -> do
      -- update the NoteInfoCache based on the newly parsed document
      title <- Page.getTitle parsed `onNothing` returnEarly ()
      filepath <- nuriToFilePath uri `onNothing` returnEarly ()
      slug <- Slug.fromMarkdownFilePath filepath `onNothing` returnEarly ()
      modify $ IxSet.updateIx slug NoteInfo {slug, title}

textDocumentDidOpen ::
  (LSP :> es, VFSAccess :> es, Diagnostics :> es, State NoteInfoCache :> es) =>
  TNotificationMessage 'Method_TextDocumentDidOpen -> Eff es ()
textDocumentDidOpen = onDocumentChange . uriFromMessage

textDocumentDidChange ::
  (LSP :> es, VFSAccess :> es, Diagnostics :> es, State NoteInfoCache :> es) =>
  TNotificationMessage 'Method_TextDocumentDidChange -> Eff es ()
textDocumentDidChange = onDocumentChange . uriFromMessage

-- | This handler is necessary to avoid logging a warning to clients:
-- @LSP: No handler for 'textDocument/didClose'@
textDocumentDidClose ::
  (LSP :> es, VFSAccess :> es, Diagnostics :> es) =>
  TNotificationMessage 'Method_TextDocumentDidClose -> Eff es ()
textDocumentDidClose _ = pure ()
