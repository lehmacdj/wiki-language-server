module Handlers.TextDocument.Completion where

import Effectful.State.Static.Shared
import Handlers.Prelude
import Models.Completion
import Models.NoteInfo
import MyPrelude
import Utils.RangePosition
import Models.Slug qualified as Slug
import Models.Page.Utils qualified as Page
import Language.LSP.Protocol.Lens qualified as J
import Effectful.FileSystem

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

throwUnableToExtractOriginalCompletion ::
  (Logging :> es, Error (TResponseError Method_CompletionItemResolve) :> es) =>
  Text -> Eff es a
throwUnableToExtractOriginalCompletion err = do
  let msg = "Can't extract original completion; failed with error: " <> err
  logError msg
  throwError_
    $ TResponseError
      { _code = InR ErrorCodes_InvalidRequest,
        _message = msg,
        _xdata = Nothing
      }

completionItemResolve ::
  ( VFSAccess :> es,
    Logging :> es,
    FileSystem :> es,
    Diagnostics :> es,
    State NoteInfoCache :> es
  ) =>
  HandlerFor 'Method_CompletionItemResolve es
completionItemResolve TRequestMessage{_params = completionItem} = do
  completion <- extractOriginalCompletion completionItem 
    `onLeft` throwUnableToExtractOriginalCompletion
  currentDirectory <- getCurrentDirectory
  let uri = Slug.intoUri currentDirectory completion.slug
  (_, mcontents) <- tryGetUriContents uri
  contents <- mcontents `onNothing` throwNoContentsAvailable
  parsed <- parseDocumentThrow uri contents
  firstH1Position@(Position l _) <- 
    Page.getFirstH1Position parsed `onNothing` throwNoContentsAvailable
  let range = Range firstH1Position (Position (l + 1000) 0)
  let notePreview = MarkupContent MarkupKind_Markdown (contents `restrictToRange` range)
  let resolved = 
        completionItem 
        & J.documentation ?~ InR notePreview
  logInfo $ "Resolved completion: " <> tshow resolved
  pure resolved
