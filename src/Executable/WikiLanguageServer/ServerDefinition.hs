module Executable.WikiLanguageServer.ServerDefinition (serverDefinition) where

import Executable.WikiLanguageServer.Interpreter
import Handlers.Initialized
import Handlers.Prelude
import Handlers.TextDocument.Changes
import Handlers.TextDocument.Completion
import Handlers.TextDocument.Definition
import Handlers.TextDocument.Formatting
import Language.LSP.Server
import Models.NoteInfo.CollectIO
import Models.WikiLanguageServerConfig
import MyPrelude
import Paths_wiki_language_server (version)

-- | Convert any synchronous exception into a TResponseError
rootExceptionHandler ::
  (Logging :> es, Error (TResponseError method) :> es) =>
  Eff es a ->
  Eff es a
rootExceptionHandler action =
  action `catchAny` \e -> do
    let msg = "Encountered unrecoverable error during request: " <> tshow e
    logError msg
    throwError_
      $ TResponseError
        { _code = InR ErrorCodes_InternalError,
          _message = msg,
          _xdata = Nothing
        }

-- | Shim for making requestHandler easier to use in the simple way that one
-- generally wants to use it
requestHandler' ::
  forall (m :: Method 'ClientToServer 'Request) es.
  (Logging :> es) =>
  ( SMethod m ->
    (TRequestMessage m -> Eff (Error (TResponseError m) : es) (MessageResult m)) ->
    Handlers (Eff es)
  )
requestHandler' s handler = requestHandler s \request responder -> do
  runErrorNoCallStack (rootExceptionHandler (handler request)) >>= responder

handlers :: Handlers (Eff Effects)
handlers =
  mconcat
    [ notificationHandler SMethod_Initialized initialized,
      notificationHandler SMethod_TextDocumentDidOpen textDocumentDidOpen,
      notificationHandler SMethod_TextDocumentDidChange textDocumentDidChange,
      requestHandler' SMethod_TextDocumentDefinition textDocumentDefinition,
      requestHandler' SMethod_TextDocumentFormatting textDocumentFormatting,
      requestHandler' SMethod_TextDocumentCompletion textDocumentCompletion
    ]

interpretHandler_ ::
  (LanguageContextEnv Config, SharedState) -> Eff Effects <~> IO
interpretHandler_ (env, sharedState) =
  Iso (runEffects env sharedState) liftIO

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
      doInitialize = \env _ -> do
        sharedState <- makeSharedState []
        noteInfos <- runEffects_ env sharedState collectNoteInfoForAllNotes
        sharedState' <- makeSharedState noteInfos
        pure (Right (env, sharedState')),
      staticHandlers = const handlers,
      interpretHandler = interpretHandler_,
      options = serverOptions
    }
