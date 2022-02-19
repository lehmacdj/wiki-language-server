-- 99.9% of functions in this module have type Handlers m, and it is redundant
-- to specify that every time.
{-# OPTIONS_GHC -Wno-missing-signatures #-}

-- | Handlers for the LSP client methods.
module Wiki.LSP.Handlers (handlers) where

import Language.LSP.Server
import Language.LSP.Types
import MyPrelude
import Wiki.LSP.Config
import Wiki.Page qualified as Page
import Language.LSP.Types.Lens as J
import Wiki.LSP.Util
import Wiki.Diagnostics

type HandlerMonad m = (MonadLsp Config m)

initialized ::
  HandlerMonad m => NotificationMessage 'Initialized -> m ()
initialized _n = pure ()

textDocumentDidOpen ::
  HandlerMonad m => NotificationMessage 'TextDocumentDidOpen -> m ()
textDocumentDidOpen notification = do
  let doc = notification ^. J.params . J.textDocument
      uri = doc ^. J.uri
      nuri = toNormalizedUri uri
      version = doc ^. J.version . to Just
      contents = doc ^. J.text
  case Page.parse (fromMaybe "<unknown>" $ uriToFilePath uri) contents of
    Left d -> do
      sendDiagnostics nuri version [d]
    Right _ ->
      sendDiagnostics
        nuri
        version
        [mkDiagnostic GeneralInfo (atLineCol 0 0) "Parsed successfully!"]

handlers :: HandlerMonad m => Handlers m
handlers =
  mconcat
    [ notificationHandler SInitialized initialized,
      notificationHandler STextDocumentDidOpen textDocumentDidOpen
    ]
