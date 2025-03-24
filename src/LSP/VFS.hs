module LSP.VFS where

import Colog.Core (Severity (..), WithSeverity (..))
import LSP.Raw
import Language.LSP.Logging qualified as Logging
import Language.LSP.Protocol.Types
import Language.LSP.Server qualified as Server
import Language.LSP.VFS (VFS, VirtualFile, rangeLinesFromVfs)
import MyPrelude
import Utils.RangePosition

-- | Extra actions exposed by underlying LSP monad that we don't use:
data VFSAccess :: Effect where
  GetVirtualFile :: NormalizedUri -> VFSAccess m (Maybe VirtualFile)
  GetVirtualFiles :: VFSAccess m VFS
  PersistVirtualFile :: FilePath -> NormalizedUri -> VFSAccess m (Maybe FilePath)
  GetVersionedTextDoc :: TextDocumentIdentifier -> VFSAccess m VersionedTextDocumentIdentifier
  ReverseFileMap :: VFSAccess m (FilePath -> FilePath)

makeEffect ''VFSAccess

getVirtualFileRange ::
  (VFSAccess :> es) => NormalizedUri -> Range -> Eff es (Maybe Text)
getVirtualFileRange uri (Range (Position ls cs) (Position le ce)) = withEarlyReturn do
  vf <- getVirtualFile uri `onNothingM` returnEarly Nothing
  let textLines = rangeLinesFromVfs vf (Range (Position ls 0) (Position (le + 1) 0))
  pure . Just $ textLines `restrictToRange` Range (Position 0 cs) (Position (le - ls) ce)

runVFSAccess :: (LSP :> es, IOE :> es) => Eff (VFSAccess : es) a -> Eff es a
runVFSAccess = interpret_ \case
  GetVirtualFile uri -> Server.getVirtualFile uri
  GetVirtualFiles -> Server.getVirtualFiles
  PersistVirtualFile path uri ->
    let logAction = contramap ((`WithSeverity` Info) . tshow) Logging.logToLogMessage
     in Server.persistVirtualFile logAction path uri
  GetVersionedTextDoc tdi -> Server.getVersionedTextDoc tdi
  ReverseFileMap -> Server.reverseFileMap
