module Handlers.TextDocument.SemanticTokens where

import Handlers.Prelude
import MyPrelude hiding (Null)

-- | Stub handler returning an empty token set.
--
-- Workaround for a bug in the @lsp@ library (confirmed present in 2.7.0.1
-- and 2.8.0.0): @inferServerCapabilities@ in
-- @Language.LSP.Server.Processing@ unconditionally advertises
-- @semanticTokensProvider@ in the server's @InitializeResult@, regardless
-- of whether any semantic tokens handler is registered. Clients such as
-- Neovim consequently send @textDocument/semanticTokens/full@ and log an
-- error when the server has no handler. Registering this no-op satisfies
-- the client until we implement real semantic tokens (or the library is
-- fixed upstream).
--
-- Related: https://github.com/haskell/lsp/issues/583
textDocumentSemanticTokensFull ::
  HandlerFor 'Method_TextDocumentSemanticTokensFull es
textDocumentSemanticTokensFull _ = pure $ InR Null
