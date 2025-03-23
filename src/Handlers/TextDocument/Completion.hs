module Handlers.TextDocument.Completion where

import Data.Text (splitOn)
import Handlers.Prelude
import Language.LSP.VFS (rangeLinesFromVfs)
import Models.NoteInfo
import MyPrelude
import Text.Fuzzy qualified as Fuzzy
import Utils.LSP (rangeFromStartOfLine)

mockCompletionsCache :: [NoteInfo]
mockCompletionsCache =
  [ NoteInfo "kWp7rk0suUXd" "Hello world",
    NoteInfo "7Fu2PSiqrvz4" "Test world",
    NoteInfo "JQiVd3GmGPpP" "Some string",
    NoteInfo "acZlsJzsFs2g" "A wild unordinary herald"
  ]

textDocumentCompletion ::
  (VFSAccess :> es, Logging :> es, FileSystem :> es) =>
  HandlerFor 'Method_TextDocumentCompletion es
textDocumentCompletion request = do
  let uri = uriFromMessage request
  vf <- getVirtualFile uri `onNothingM` throwNoContentsAvailable
  let position = positionFromMessage request
  let line = rangeLinesFromVfs vf (rangeFromStartOfLine position)
  let completionPrefix = last $ "" `ncons` splitOn "[[" line
  let results = Fuzzy.filter completionPrefix mockCompletionsCache "" "" (.title) False
  pure . InL $ results <&> \(Fuzzy.Fuzzy NoteInfo {..} _ _) ->
    CompletionItem
      { _label = title,
        _labelDetails = Nothing,
        _kind = Just CompletionItemKind_Text,
        _tags = Nothing,
        _detail = Nothing, -- TODO: include full note contents/preview of contents?
        _documentation = Nothing, -- TODO: include full note contents/preview of contents?
        _deprecated = Nothing,
        _preselect = Nothing,
        _sortText = Just title,
        _filterText = Just title,
        _insertText = Nothing,
        _insertTextFormat = Nothing,
        _insertTextMode = Just InsertTextMode_AsIs,
        _textEdit = Just . InL $ TextEdit (Range position position) $ slug ++ "|" ++ title ++ "]]<!--wls-->", -- TODO: maybe also generate an edit that keeps the title as is and doesn't replace it, not appending <!--wls-->?
        _textEditText = Nothing, -- TODO: probably worth factoring out the textEditRange into the default text edit for the completion
        _additionalTextEdits = Nothing,
        _commitCharacters = Nothing,
        _command = Nothing,
        _data_ = Nothing
      }
