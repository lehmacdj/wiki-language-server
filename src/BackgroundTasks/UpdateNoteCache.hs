module BackgroundTasks.UpdateNoteCache where

import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as LBS
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
  updateNoteCacheTaskDelay <- inputs (.updateNoteCacheTaskDelay)
  sleep updateNoteCacheTaskDelay -- maybe worth jittering?

saveNoteInfoCache ::
  (IOE :> es, Logging :> es) =>
  NoteInfoCache ->
  Eff es ()
saveNoteInfoCache cache = do
  cachePath <- liftIO $ statePath noteCacheFileName
  let contents =
        LBS.toStrict $ Builder.toLazyByteString $ serializeNoteInfoCache cache
  atomicWriteFile cachePath contents
    `catch` \(e :: SomeException) ->
      logWarn $ "Failed to save note cache: " <> tshow e
