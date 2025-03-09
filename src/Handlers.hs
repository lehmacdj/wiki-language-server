-- | Handlers for the LSP client methods.
module Handlers (handlers) where

import Handlers.Initialized
import Handlers.Prelude
import Handlers.TextDocument.Changes
import Handlers.TextDocument.Definition
import Handlers.TextDocument.Formatting
import Models.WikiLanguageServerConfig
import MyPrelude

-- | Shim for making requestHandler easier to use in the simple way that one
-- generally wants to use it
requestHandler' ::
  forall (m :: Method 'ClientToServer 'Request) f.
  (Monad f) =>
  SMethod m ->
  (TRequestMessage m -> f (Either (TResponseError m) (MessageResult m))) ->
  Handlers f
requestHandler' s handler = requestHandler s \request responder -> do
  handler request >>= responder

handlers :: (MonadLsp Config m) => Handlers m
handlers =
  mconcat
    [ notificationHandler SMethod_Initialized initialized,
      notificationHandler SMethod_TextDocumentDidOpen textDocumentDidOpen,
      notificationHandler SMethod_TextDocumentDidChange textDocumentDidChange,
      requestHandler' SMethod_TextDocumentDefinition textDocumentDefinition,
      requestHandler' SMethod_TextDocumentFormatting textDocumentFormatting
    ]
