module Executable.WikiLanguageServer (main) where

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

main :: IO ()
main = runGlobalEffects_ main_
