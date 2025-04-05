module Models.Completion where

import Data.Text (breakOn)
import Language.LSP.Protocol.Types
import Models.NoteInfo
import Models.Slug
import MyPrelude
import Text.Fuzzy (Fuzzy (Fuzzy))
import Text.Fuzzy qualified as Fuzzy
import Utils.RangePosition
import Utils.Text

data Completion
  = WikiLinkCompletion
  { replaceRange :: Range,
    slug :: Slug,
    title :: Text,
    label :: Text,
    typedAlias :: Text
  }
  deriving (Show, Eq, Generic)

makeWikiLinkCompletionsFromLine :: NoteInfoCache -> Position -> Text -> [Completion]
makeWikiLinkCompletionsFromLine noteInfos position line = withEarlyReturn_ do
  let reversed = reverse line
      (reversedPrefix, reversedRest) = breakOn "[[" reversed
  when ("]]" `isInfixOf` reversedPrefix) $ returnEarly []
  let prefix = reverse reversedPrefix
      (start, end) = breakOn "|" prefix
      completionPrefix = if null end then start else end
  pure
    $ Fuzzy.filter completionPrefix (toList noteInfos) "" "" (.title) False
    <&> \(Fuzzy NoteInfo {..} _ _) ->
      WikiLinkCompletion
        { replaceRange = Range (sameLineWithCol position (length reversedRest)) position,
          slug,
          title,
          label = title ++ " (" ++ slug.text ++ ")",
          typedAlias = prefix
        }

renderCompletionForLineNum :: Completion -> [CompletionItem]
renderCompletionForLineNum WikiLinkCompletion {..} =
  [ TextEdit replaceRange $ slug.text ++ "|" ++ title ++ "]]<!--wls-->"
  -- Somewhat appealing to expose such a completion too:
  -- TextEdit replaceRange $ "[[" ++ slug ++ "|" ++ typedAlias ++ "]]"
  ]
    <&> \edit ->
      CompletionItem
        { _label = truncateWithMarker 60 "…" title,
          _labelDetails =
            Just
              CompletionItemLabelDetails
                { _detail = Just slug.text,
                  _description = Nothing
                },
          _kind = Just CompletionItemKind_File,
          _tags = Nothing,
          _detail = Just slug.text,
          _documentation = Nothing, -- TODO: resolve request to get full note text
          _deprecated = Nothing,
          _preselect = Nothing,
          _sortText = Just title,
          _filterText = Just title,
          _insertText = Nothing,
          _insertTextFormat = Nothing,
          _insertTextMode = Just InsertTextMode_AsIs,
          _textEdit = Just . InL $ edit,
          _textEditText = Nothing,
          _additionalTextEdits = Nothing,
          _commitCharacters = Nothing,
          _command = Nothing,
          _data_ = Nothing
        }
