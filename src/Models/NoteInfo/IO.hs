module Models.NoteInfo.IO where

import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as LBS
import Data.IxSet.Typed qualified as IxSet
import Data.Text.Encoding qualified as Text
import Effectful.FileSystem
import Handlers.Prelude
import Models.NoteInfo
import Models.NoteInfo.Serialization
  ( deserializeNoteInfoCache,
    serializeNoteInfoCache,
  )
import Models.Page.Utils qualified as Page
import Models.Slug (Slug)
import Models.Slug qualified as Slug
import MyPrelude
import Utils.XdgPaths (statePath)

-- | File name for the serialized cache
noteCacheFileName :: FilePath
noteCacheFileName = "note-cache.tsv"

-- | File name for the day notes cache (filtered view)
dayNoteCacheFileName :: FilePath
dayNoteCacheFileName = "day-note-cache.tsv"

collectNoteInfoForAllNotes ::
  (VFSAccess :> es, Logging :> es, FileSystem :> es, Diagnostics :> es) =>
  Eff es NoteInfoCache
collectNoteInfoForAllNotes = do
  directoryContents <- listDirectory "."
  currentDirectory <- getCurrentDirectory
  results <- for directoryContents \filePath -> withEarlyReturn do
    slug <-
      Slug.fromMarkdownFilePath filePath
        `onNothing` returnEarly Nothing
    let uri = Slug.intoUri currentDirectory slug
    collectNoteInfoForSlugWithUri slug uri
  pure . IxSet.fromList $ catMaybes results

collectNoteInfoForSlugWithUri ::
  (VFSAccess :> es, Logging :> es, FileSystem :> es, Diagnostics :> es) =>
  Slug -> NormalizedUri -> Eff es (Maybe NoteInfo)
collectNoteInfoForSlugWithUri slug uri = withEarlyReturn do
  (version, mContents) <- tryGetUriContents uri
  contents <- mContents `onNothing` returnEarly Nothing
  parsed <-
    parseDocument uri contents `onLeft` \diagnostic -> do
      sendDiagnostics uri version [diagnostic]
      returnEarly Nothing
  title <- Page.getTitle parsed `onNothing` returnEarly Nothing
  let day = Page.dayNoteTitleToDay title
  pure $ Just $ NoteInfo {..}

loadCache :: (IOE :> es) => Eff es NoteInfoCache
loadCache = do
  cachePath <- liftIO $ statePath noteCacheFileName
  result <-
    liftIO $ try @_ @SomeException $ BS.readFile cachePath
  case result of
    Left _ -> pure IxSet.empty
    Right bs -> case deserializeNoteInfoCache bs of
      Left _ -> pure IxSet.empty
      Right cache -> pure cache

-- | Serialize a NoteInfoCache and write it to an XDG state file.
saveCacheToFile ::
  (IOE :> es, Logging :> es) =>
  FilePath ->
  Text ->
  NoteInfoCache ->
  Eff es ()
saveCacheToFile fileName description cache = do
  cachePath <- liftIO $ statePath fileName
  let contents =
        LBS.toStrict $
          Builder.toLazyByteString $
            serializeNoteInfoCache cache
  atomicWriteFile cachePath contents
    `catch` \(e :: SomeException) ->
      logWarn $
        "Failed to save " <> description <> ": " <> tshow e

saveCache ::
  (IOE :> es, Logging :> es) =>
  NoteInfoCache ->
  Eff es ()
saveCache = saveCacheToFile noteCacheFileName "note cache"

saveDayNoteCache ::
  (IOE :> es, Logging :> es) =>
  NoteInfoCache ->
  Eff es ()
saveDayNoteCache cache =
  saveCacheToFile dayNoteCacheFileName "day note cache" dayNotes
  where
    dayNotes = cache IxSet.@> (Nothing :: Maybe Day)

rescanCache ::
  ( VFSAccess :> es,
    Logging :> es,
    FileSystem :> es,
    Diagnostics :> es,
    IOE :> es
  ) =>
  Eff es NoteInfoCache
rescanCache = do
  cache <- collectNoteInfoForAllNotes
  saveCache cache
  pure cache

-- | Create a new note file for the given day and return its
-- NoteInfo. The file is written relative to the current
-- directory.
createDateNote :: (FileSystem :> es, IOE :> es) => Day -> Eff es NoteInfo
createDateNote day = do
  slug <- liftIO Slug.generateRandomSlug
  now <- liftIO getZonedTime
  let title =
        pack $ formatTime defaultTimeLocale "%Y-%m-%d" day
      dateStr =
        formatTime
          defaultTimeLocale
          "%Y-%m-%dT%H:%M:%S%Ez"
          now
      content =
        unlines
          [ "---",
            "date: " <> pack dateStr,
            "---",
            "",
            "# " <> title
          ]
      filePath =
        Slug.intoFilePathRelativeToDir "." slug.text
  liftIO $
    BS.writeFile filePath (Text.encodeUtf8 content)
  pure NoteInfo {day = Just day, ..}
