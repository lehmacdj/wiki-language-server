module Handlers.TextDocument.Formatting where

import Data.IxSet.Typed qualified as IxSet
import Effectful.FileSystem (getCurrentDirectory)
import Effectful.State.Static.Shared
import Handlers.Prelude
import Models.NoteInfo
import Models.NoteInfo.CollectIO
import Models.Page.Formatting qualified as Formatting
import Models.Slug (Slug)
import Models.Slug qualified as Slug
import MyPrelude

-- | Get the title for a slug returning Nothing if the file isn't found or we
-- can't find the title in the page
titleForSlug ::
  ( Logging :> es,
    VFSAccess :> es,
    FileSystem :> es,
    State NoteInfoCache :> es,
    Diagnostics :> es
  ) =>
  Slug -> Eff es (Maybe Text)
titleForSlug slug = withEarlyReturn do
  currentDirectory <- getCurrentDirectory
  let uri = Slug.intoUri currentDirectory slug
  -- might be sensible to tune this e.g. by invalidating the cache if the
  -- cache was updated later than the file etc. but treating the cache as
  -- accurate is probably fine?
  withJustM (gets (IxSet.getOne . IxSet.getEQ slug))
    $ returnEarly
    . Just
    . (.title)
  noteInfo@NoteInfo {..} <-
    collectNoteInfoForSlugWithUri slug uri `onNothingM` returnEarly Nothing
  modify $ IxSet.updateIx slug noteInfo
  pure $ Just title

textDocumentFormatting ::
  ( Logging :> es,
    VFSAccess :> es,
    FileSystem :> es,
    State NoteInfoCache :> es,
    Diagnostics :> es
  ) =>
  HandlerFor 'Method_TextDocumentFormatting es
textDocumentFormatting request = do
  let uri = uriFromMessage request
  mVersionContents <- tryGetVfsUriContents uri
  (_, contents) <- onNothing mVersionContents throwNoContentsAvailable
  parsed <- parseDocumentThrow uri contents
  let edits = Formatting.editsForPage parsed
  resolvedEdits <- traverse (Formatting.textEditOfOperation titleForSlug) edits
  pure . InL $ concat resolvedEdits
