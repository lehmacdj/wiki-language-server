module Executable.LSP (main) where

import Language.LSP.Server
import MyPrelude
import System.Exit
import Wiki.LSP.ServerDefinition

exitCodeFromInt :: Int -> ExitCode
exitCodeFromInt = \case
  0 -> ExitSuccess
  n -> ExitFailure n

main :: IO ()
main = runServer serverDefinition >>= exitWith . exitCodeFromInt
