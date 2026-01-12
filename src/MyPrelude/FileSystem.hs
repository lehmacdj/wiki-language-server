module MyPrelude.FileSystem
  ( -- * Temporary effect
    Temporary (..),
    withTempFile,
    runTemporary,

    -- * Atomic file operations
    atomicWriteFile,
  )
where

import Data.ByteString qualified as BS
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.TH
import MyPrelude.RestrictedClassyPrelude hiding (withTempFile)
import System.Directory (createDirectoryIfMissing, renameFile)
import System.FilePath (takeDirectory)
import System.IO (Handle)

-- | Effect for temporary file operations
data Temporary :: Effect where
  WithTempFile ::
    -- | Template for temp file name
    String ->
    -- | Action to run with temp file
    (FilePath -> Handle -> m a) ->
    Temporary m a

makeEffect ''Temporary

-- | Run Temporary effect using actual system temp files
runTemporary :: (IOE :> es) => Eff (Temporary : es) a -> Eff es a
runTemporary = interpret \env -> \case
  WithTempFile template action ->
    localSeqUnliftIO env \unlift ->
      withSystemTempFile template \path handle ->
        unlift $ action path handle

-- | Atomically write a file by writing to a temp file first, then renaming.
-- This ensures the target file is never in a partial/corrupt state.
-- Creates parent directories if they don't exist.
-- Uses a temp file in the same directory to ensure atomic rename on POSIX.
atomicWriteFile ::
  (IOE :> es) =>
  FilePath ->
  ByteString ->
  Eff es ()
atomicWriteFile targetPath contents = liftIO do
  createDirectoryIfMissing True (takeDirectory targetPath)
  let tempPath = targetPath <> ".tmp"
  BS.writeFile tempPath contents
  renameFile tempPath targetPath
