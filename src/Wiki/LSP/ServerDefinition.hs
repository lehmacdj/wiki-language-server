module Wiki.LSP.ServerDefinition (serverDefinition) where

import Language.LSP.Server
import MyPrelude
import Wiki.LSP.Config
import Wiki.LSP.Handlers

type HandlerM = LspT Config IO

runHandler :: LanguageContextEnv Config -> HandlerM a -> IO a
runHandler = runLspT

handlerInterpreter :: LanguageContextEnv Config -> HandlerM <~> IO
handlerInterpreter env = Iso (runHandler env) liftIO

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
