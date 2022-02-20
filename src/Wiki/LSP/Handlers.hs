-- 99.9% of functions in this module have type Handlers m, and it is redundant
-- to specify that every time.
{-# OPTIONS_GHC -Wno-missing-signatures #-}

-- | Handlers for the LSP client methods.
module Wiki.LSP.Handlers (handlers) where

import Language.LSP.Server
import Language.LSP.Types
import Language.LSP.Types.Lens as J
import Language.LSP.VFS
import MyPrelude
import Wiki.Diagnostics
import Wiki.LSP.Config
import Wiki.LSP.Util
import Wiki.Page qualified as Page

type HandlerMonad m = (MonadLsp Config m)

initialized ::
  HandlerMonad m => NotificationMessage 'Initialized -> m ()
initialized _n = pure ()

textDocumentDidOpen ::
  HandlerMonad m => NotificationMessage 'TextDocumentDidOpen -> m ()
textDocumentDidOpen notification = do
  let doc = notification ^. J.params . J.textDocument
      nuri = doc ^. J.uri . to toNormalizedUri
      version = doc ^. J.version . to Just
      contents = doc ^. J.text
  case Page.parse (fromMaybe "<unknown>" $ nuriToFilePath nuri) contents of
    Left d ->
      sendDiagnostics nuri version [d]
    Right _ ->
      sendDiagnostics
        nuri
        version
        [mkDiagnostic GeneralInfo (atLineCol 0 0) "didOpen: Parsed successfully!"]

textDocumentDidChange ::
  HandlerMonad m => NotificationMessage 'TextDocumentDidChange -> m ()
textDocumentDidChange notification = do
  let nuri =
        notification
          ^. J.params
            . J.textDocument
            . J.uri
            . to toNormalizedUri
  getVirtualFile nuri >>= \case
    Nothing ->
      -- failed to get document
      pure ()
    Just vf -> do
      let version = Just $ virtualFileVersion vf
          contents = virtualFileText vf
      case Page.parse (fromMaybe "<unknown>" $ nuriToFilePath nuri) contents of
        Left d ->
          sendDiagnostics nuri version [d]
        Right _ ->
          sendDiagnostics
            nuri
            version
            [mkDiagnostic GeneralInfo (atLineCol 0 0) "didChange: Parsed successfully!"]

handlers :: HandlerMonad m => Handlers m
handlers =
  mconcat
    [ notificationHandler SInitialized initialized,
      notificationHandler STextDocumentDidOpen textDocumentDidOpen,
      notificationHandler STextDocumentDidChange textDocumentDidChange
    ]
