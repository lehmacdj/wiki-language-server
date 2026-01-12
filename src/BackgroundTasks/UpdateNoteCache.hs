module BackgroundTasks.UpdateNoteCache where

import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as LBS
import Data.IxSet.Typed qualified as IxSet
import Effectful.State.Static.Shared
import Handlers.Prelude
import Models.NoteInfo
import Models.NoteInfo.CollectIO
import Models.NoteInfo.Serialization (serializeNoteInfoCache)
import MyPrelude
import Utils.XdgPaths (statePath)

-- | File name for the serialized cache
noteCacheFileName :: FilePath
noteCacheFileName = "note-cache.tsv"

-- | File name for the day notes cache (filtered view)
dayNoteCacheFileName :: FilePath
dayNoteCacheFileName = "day-note-cache.tsv"

updateNoteCacheTask ::
  ( Concurrent :> es,
    VFSAccess :> es,
    Logging :> es,
    FileSystem :> es,
    Input Config :> es,
    Diagnostics :> es,
    State NoteInfoCache :> es,
    IOE :> es
  ) =>
  Eff es Void
updateNoteCacheTask = forever do
  noteInfos <- collectNoteInfoForAllNotes
  put noteInfos
  saveNoteInfoCache noteInfos
  saveDayNoteCache noteInfos
  updateNoteCacheTaskDelay <- inputs (.updateNoteCacheTaskDelay)
  sleep updateNoteCacheTaskDelay -- maybe worth jittering?

saveNoteInfoCache ::
  (IOE :> es, Logging :> es) =>
  NoteInfoCache ->
  Eff es ()
saveNoteInfoCache = saveCacheToFile noteCacheFileName "note cache"

saveDayNoteCache ::
  (IOE :> es, Logging :> es) =>
  NoteInfoCache ->
  Eff es ()
saveDayNoteCache cache =
  saveCacheToFile dayNoteCacheFileName "day note cache" dayNotes
  where
    dayNotes = cache IxSet.@> (Nothing :: Maybe Day)

saveCacheToFile ::
  (IOE :> es, Logging :> es) =>
  FilePath ->
  Text ->
  NoteInfoCache ->
  Eff es ()
saveCacheToFile fileName description cache = do
  cachePath <- liftIO $ statePath fileName
  let contents =
        LBS.toStrict $ Builder.toLazyByteString $ serializeNoteInfoCache cache
  atomicWriteFile cachePath contents
    `catch` \(e :: SomeException) ->
      logWarn $ "Failed to save " <> description <> ": " <> tshow e
