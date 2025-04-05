module Handlers.TextDocument.Completion where

import Effectful.State.Static.Shared
import Handlers.Prelude
import Models.Completion
import Models.NoteInfo
import MyPrelude
import Utils.RangePosition

textDocumentCompletion ::
  ( VFSAccess :> es,
    Logging :> es,
    FileSystem :> es,
    Diagnostics :> es,
    State NoteInfoCache :> es
  ) =>
  HandlerFor 'Method_TextDocumentCompletion es
textDocumentCompletion request = do
  let uri = uriFromMessage request
  let position = positionFromMessage request
  line <-
    getVirtualFileRange uri (rangeFromStartOfLine position)
      `onNothingM` throwNoContentsAvailable
  noteInfos <- get
  let completions = makeWikiLinkCompletionsFromLine noteInfos position line
  let result =
        CompletionList
          { _isIncomplete = True,
            _itemDefaults = Nothing,
            _items = renderCompletionForLineNum =<< completions
          }
  pure . InR . InL $ result
