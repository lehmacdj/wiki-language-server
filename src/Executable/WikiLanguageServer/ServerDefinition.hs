module Executable.WikiLanguageServer.ServerDefinition (serverDefinition) where

import Executable.WikiLanguageServer.Interpreter
import Handlers.Initialized
import Handlers.Prelude
import Handlers.TextDocument.Changes
import Handlers.TextDocument.Definition
import Handlers.TextDocument.Formatting
import Language.LSP.Server
import Models.WikiLanguageServerConfig
import MyPrelude
import Paths_wiki_language_server (version)

-- | Shim for making requestHandler easier to use in the simple way that one
-- generally wants to use it
requestHandler' ::
  forall (m :: Method 'ClientToServer 'Request) es.
  ( SMethod m ->
    (TRequestMessage m -> Eff (Error (TResponseError m) : es) (MessageResult m)) ->
    Handlers (Eff es)
  )
requestHandler' s handler = requestHandler s \request responder -> do
  runErrorNoCallStack (handler request) >>= responder

handlers :: Handlers (Eff Effects)
handlers =
  mconcat
    [ notificationHandler SMethod_Initialized initialized,
      notificationHandler SMethod_TextDocumentDidOpen textDocumentDidOpen,
      notificationHandler SMethod_TextDocumentDidChange textDocumentDidChange,
      requestHandler' SMethod_TextDocumentDefinition textDocumentDefinition,
      requestHandler' SMethod_TextDocumentFormatting textDocumentFormatting
    ]

interpretHandler_ ::
  LanguageContextEnv Config -> Eff Effects <~> IO
interpretHandler_ env = Iso (runEffects env) liftIO

serverOptions :: Options
serverOptions =
  def
    { optTextDocumentSync =
        Just
          TextDocumentSyncOptions
            { _openClose = Just True,
              _change = Just TextDocumentSyncKind_Incremental,
              _willSave = Nothing,
              _willSaveWaitUntil = Nothing,
              _save = Nothing
            },
      optServerInfo =
        Just
          $ ServerInfo "wiki-language-server" (Just $ tshow version)
    }

serverDefinition :: ServerDefinition Config
serverDefinition =
  ServerDefinition
    { defaultConfig = def,
      configSection = "wiki-language-server",
      parseConfig = Models.WikiLanguageServerConfig.parseConfig,
      onConfigChange = const $ pure (),
      doInitialize = \env _ -> pure (Right env),
      staticHandlers = const handlers,
      interpretHandler = interpretHandler_,
      options = serverOptions
    }
