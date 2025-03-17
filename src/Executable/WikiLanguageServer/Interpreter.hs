module Executable.WikiLanguageServer.Interpreter where

import Effectful.FileSystem (runFileSystem)
import LSP.Raw
import LSP.VFS
import Language.LSP.Server
import Models.WikiLanguageServerConfig
import MyPrelude
import Utils.Logging

type Effects =
  [ VFSAccess,
    Logging,
    LSP,
    FileSystem,
    IOE
  ]

runEffects_ :: LanguageContextEnv Config -> Eff Effects a -> IO a
runEffects_ config =
  runVFSAccess
    >>> runLoggingLSP
    >>> runLSP config
    >>> runFileSystem
    >>> runEff

runEffects ::
  (Subset es Effects) =>
  LanguageContextEnv Config -> Eff es a -> IO a
runEffects config action = runEffects_ config $ inject action
