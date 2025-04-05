module Executable.WikiLanguageServer.Interpreter where

import Effectful.FileSystem (runFileSystem)
import Effectful.State.Static.Shared
import LSP.Raw
import LSP.VFS
import Language.LSP.Server
import Models.NoteInfo
import Models.WikiLanguageServerConfig
import MyPrelude
import Utils.Diagnostics
import Utils.Logging

-- | These effects need to be re-run with the LSP env
type LSPEffects :: [Effect]
type LSPEffects =
  [ VFSAccess,
    Logging,
    Diagnostics,
    Input Config,
    LSP
  ]

-- | These effects are available in the global env that we run in main
type GlobalEffects :: [Effect]
type GlobalEffects =
  [ FileSystem,
    State [NoteInfo],
    Concurrent,
    IOE
  ]

type family (++) (es :: [k]) (es' :: [k]) :: [k] where
  '[] ++ es = es
  (e ': es) ++ es' = e ': (es ++ es')

type Effects :: [Effect]
type Effects = LSPEffects ++ GlobalEffects

runLSPEffects_ ::
  (IOE :> es) =>
  LanguageContextEnv Config -> Eff (LSPEffects ++ es) a -> Eff es a
runLSPEffects_ config =
  id -- this is here so >>> can precede all following lines
    >>> runVFSAccess
    >>> runLoggingLSP
    >>> runDiagnostics
    >>> runInputViaGetter getConfig
    >>> runLSP config

runGlobalEffects_ ::
  Eff GlobalEffects a -> IO a
runGlobalEffects_ =
  id -- this is here so >>> can precede all following lines
    >>> runFileSystem
    >>> evalState []
    >>> runConcurrent
    >>> runEff
