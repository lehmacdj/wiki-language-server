module Executable.WikiLanguageServer.Interpreter where

import Control.Concurrent.MVar.Strict
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

type Effects =
  [ State [NoteInfo],
    VFSAccess,
    Logging,
    Diagnostics,
    LSP,
    FileSystem,
    IOE
  ]

-- | A hack that is necessary because our interpreter needs to be called
-- multiple times for each handle. We run state effects using a concrete MVar
-- and initialize/pass the MVars when calling @doInitialize@.
newtype SharedState = SharedState
  { noteInfos :: MVar' [NoteInfo]
  }

makeSharedState :: [NoteInfo] -> IO SharedState
makeSharedState noteInfos = do
  noteInfos <- newMVar' noteInfos
  pure $ SharedState {..}

runEffects_ :: LanguageContextEnv Config -> SharedState -> Eff Effects a -> IO a
runEffects_ config SharedState {..} =
  id -- this is here so >>> can precede all following lines
    >>> evalStateMVar noteInfos
    >>> runVFSAccess
    >>> runLoggingLSP
    >>> runDiagnostics
    >>> runLSP config
    >>> runFileSystem
    >>> runEff

runEffects ::
  (Subset es Effects) =>
  LanguageContextEnv Config -> SharedState -> Eff es a -> IO a
runEffects config sharedState action =
  runEffects_ config sharedState $ inject action
