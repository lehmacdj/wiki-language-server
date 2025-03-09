module DAL.VFS where

import Language.LSP.Protocol.Types
import Language.LSP.VFS (VirtualFile)
import MyPrelude

-- | Extra actions exposed by underlying LSP monad that we don't use:
-- GetVirtualFiles :: VFSAccess m VFS
-- PersistVirtualFile :: LogAction m (WithSeverity VfsLog) -> FilePath -> NormalizedUri -> VFSAccess m (Maybe FilePath)
-- GetVersionedTextDoc :: TextDocumentIdentifier -> VFSAccess m VersionedTextDocumentIdentifier
-- ReverseFileMap :: VFSAccess m (FilePath -> FilePath)
data VFSAccess :: Effect where
  GetVirtualFile :: NormalizedUri -> VFSAccess m (Maybe VirtualFile)

makeEffect ''VFSAccess
