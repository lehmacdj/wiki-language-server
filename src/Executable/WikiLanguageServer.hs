module Executable.WikiLanguageServer (wikiLanguageServerMain) where

import Executable.WikiLanguageServer.Interpreter
import Executable.WikiLanguageServer.ServerDefinition
import Language.LSP.Server
import MyPrelude
import System.Exit

exitCodeFromInt :: Int -> ExitCode
exitCodeFromInt = \case
  0 -> ExitSuccess
  n -> ExitFailure n

main_ :: Eff GlobalEffects ()
main_ = do
  exitCode <- unsafeEff $ runServer . serverDefinition
  liftIO . exitWith $ exitCodeFromInt exitCode

wikiLanguageServerMain :: IO ()
wikiLanguageServerMain = runGlobalEffects_ main_
