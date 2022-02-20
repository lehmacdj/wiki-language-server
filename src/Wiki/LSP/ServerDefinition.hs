module Wiki.LSP.ServerDefinition (serverDefinition) where

import Language.LSP.Server
import Language.LSP.Types
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
    { textDocumentSync =
        Just
          TextDocumentSyncOptions
            { _openClose = Just True,
              _change = Just TdSyncIncremental,
              _willSave = Nothing,
              _willSaveWaitUntil = Nothing,
              _save = Nothing
            },
      serverInfo =
        Just $
          ServerInfo "wiki-language-server" (Just $ tshow version)
    }

serverDefinition :: ServerDefinition Config
serverDefinition =
  ServerDefinition
    { defaultConfig = def,
      onConfigurationChange = onConfigChange,
      doInitialize = \env _ -> pure (Right env),
      staticHandlers = handlers,
      interpretHandler = interpretHandlerM,
      options = serverOptions
    }
