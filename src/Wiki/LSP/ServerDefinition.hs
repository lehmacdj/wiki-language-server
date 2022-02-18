module Wiki.LSP.ServerDefinition (serverDefinition) where

import Language.LSP.Server
import Language.LSP.Types
import MyPrelude
import Paths_wiki_language_server (version)
import Wiki.LSP.Config
import Wiki.LSP.Handlers

type HandlerM = LspT Config IO

runHandler :: LanguageContextEnv Config -> HandlerM a -> IO a
runHandler = runLspT

handlerInterpreter :: LanguageContextEnv Config -> HandlerM <~> IO
handlerInterpreter env = Iso (runHandler env) liftIO

options :: Options
options =
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
    { defaultConfig = Config,
      onConfigurationChange = \_ _ -> Right Config,
      doInitialize = \env _ -> pure (Right env),
      staticHandlers = handlers,
      interpretHandler = handlerInterpreter,
      options = def
    }
