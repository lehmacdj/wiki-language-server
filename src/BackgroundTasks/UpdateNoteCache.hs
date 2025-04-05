module BackgroundTasks.UpdateNoteCache where

import Effectful.State.Static.Shared
import Handlers.Prelude
import Models.NoteInfo
import Models.NoteInfo.CollectIO
import MyPrelude

updateNoteCacheTask ::
  ( Concurrent :> es,
    VFSAccess :> es,
    Logging :> es,
    FileSystem :> es,
    Input Config :> es,
    Diagnostics :> es,
    State [NoteInfo] :> es
  ) =>
  Eff es Void
updateNoteCacheTask = forever do
  noteInfos <- collectNoteInfoForAllNotes
  put noteInfos
  updateNoteCacheTaskDelay <- inputs (.updateNoteCacheTaskDelay)
  sleep updateNoteCacheTaskDelay -- maybe worth jittering?
