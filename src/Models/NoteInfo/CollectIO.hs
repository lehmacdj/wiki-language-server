module Models.NoteInfo.CollectIO where

import Data.IxSet.Typed qualified as IxSet
import Effectful.FileSystem
import Handlers.Prelude
import Models.NoteInfo
import Models.Page.Utils qualified as Page
import Models.Slug qualified as Slug
import MyPrelude

collectNoteInfoForAllNotes ::
  (VFSAccess :> es, Logging :> es, FileSystem :> es, Diagnostics :> es) =>
  Eff es NoteInfoCache
collectNoteInfoForAllNotes = do
  directoryContents <- listDirectory "."
  currentDirectory <- getCurrentDirectory
  results <- for directoryContents \filePath -> withEarlyReturn do
    slug <- Slug.fromMarkdownFilePath filePath `onNothing` returnEarly Nothing
    let uri = Slug.intoUri currentDirectory slug
    (version, mContents) <- tryGetUriContents uri
    contents <- mContents `onNothing` returnEarly Nothing
    parsed <-
      parseDocument uri contents `onLeft` \diagnostic -> do
        sendDiagnostics uri version [diagnostic]
        returnEarly Nothing
    title <- Page.getTitle parsed `onNothing` returnEarly Nothing
    pure $ Just $ NoteInfo {..}
  pure . IxSet.fromList $ catMaybes results
