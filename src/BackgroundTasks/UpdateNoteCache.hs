module BackgroundTasks.UpdateNoteCache
  ( updateNoteCacheTask,
  )
where

import Effectful.State.Static.Shared
import Handlers.Prelude
import Models.NoteInfo
import Models.NoteInfo.IO
  ( collectNoteInfoForAllNotes,
    saveCache,
    saveDayNoteCache,
  )
import MyPrelude

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
  saveCache noteInfos
  saveDayNoteCache noteInfos
  updateNoteCacheTaskDelay <- inputs (.updateNoteCacheTaskDelay)
  sleep updateNoteCacheTaskDelay -- maybe worth jittering?
