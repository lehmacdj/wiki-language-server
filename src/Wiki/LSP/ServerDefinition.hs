module Wiki.LSP.ServerDefinition (serverDefinition) where

import Language.LSP.Protocol.Types
import Language.LSP.Server
import MyPrelude
import Paths_wiki_language_server (version)
import Wiki.LSP.Config
import Wiki.LSP.Handlers

type HandlerM = LspT Config IO

runHandlerM :: LanguageContextEnv Config -> HandlerM a -> IO a
runHandlerM = runLspT

interpretHandlerM :: LanguageContextEnv Config -> HandlerM <~> IO
interpretHandlerM env = Iso (runHandlerM env) liftIO

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
        Just $
          ServerInfo "wiki-language-server" (Just $ tshow version)
    }

serverDefinition :: ServerDefinition Config
serverDefinition =
  ServerDefinition
    { defaultConfig = def,
      configSection = "wiki-language-server",
      parseConfig = Wiki.LSP.Config.parseConfig,
      onConfigChange = const $ pure (),
      doInitialize = \env _ -> pure (Right env),
      staticHandlers = const handlers,
      interpretHandler = interpretHandlerM,
      options = serverOptions
    }
