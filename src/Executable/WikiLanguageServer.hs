module Executable.WikiLanguageServer (main) where

import Language.LSP.Server
import MyPrelude
import System.Exit
import Executable.WikiLanguageServer.ServerDefinition

exitCodeFromInt :: Int -> ExitCode
exitCodeFromInt = \case
  0 -> ExitSuccess
  n -> ExitFailure n

main :: IO ()
main = runServer serverDefinition >>= exitWith . exitCodeFromInt
