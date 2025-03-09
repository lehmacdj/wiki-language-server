module LSP.VFS where

import LSP.Raw
import Language.LSP.Protocol.Types
import Language.LSP.Server qualified as Server
import Language.LSP.VFS (VFS, VirtualFile)
import MyPrelude

-- | Extra actions exposed by underlying LSP monad that we don't use:
data VFSAccess :: Effect where
  GetVirtualFile :: NormalizedUri -> VFSAccess m (Maybe VirtualFile)
  GetVirtualFiles :: VFSAccess m VFS
  PersistVirtualFile :: FilePath -> NormalizedUri -> VFSAccess m (Maybe FilePath)
  GetVersionedTextDoc :: TextDocumentIdentifier -> VFSAccess m VersionedTextDocumentIdentifier
  ReverseFileMap :: VFSAccess m (FilePath -> FilePath)

makeEffect ''VFSAccess

runVFSAccess :: (LSP :> es) => Eff (VFSAccess : es) a -> Eff es a
runVFSAccess = interpret_ \case
  GetVirtualFile uri -> Server.getVirtualFile uri
  GetVirtualFiles -> Server.getVirtualFiles
  PersistVirtualFile path uri ->
    Server.persistVirtualFile Server.logToLogMessage path uri
  GetVersionedTextDoc tdi -> Server.getVersionedTextDoc tdi
  ReverseFileMap -> Server.reverseFileMap
