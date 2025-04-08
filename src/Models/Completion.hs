module Models.Completion where

import Data.Text (breakOn, breakOnEnd)
import Language.LSP.Protocol.Types
import Models.NoteInfo
import Models.Slug
import MyPrelude
import Text.Fuzzy (Fuzzy (Fuzzy))
import Text.Fuzzy qualified as Fuzzy
import Utils.RangePosition
import Utils.Text

-- | Extra characters that the client should trigger completion on
extraCompletionCharacters :: [Char]
extraCompletionCharacters = ['[', '|', ' ', '-']

data Completion
  = WikiLinkCompletion
  { replaceRange :: Range,
    slug :: Slug,
    title :: Text,
    label :: Text,
    typedAlias :: Text
  }
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via FastGenericEncoding Completion

wikiLinkCompletion :: Range -> Slug -> Text -> Text -> Completion
wikiLinkCompletion replaceRange slug title typedAlias =
  WikiLinkCompletion
    { label = title ++ " (" ++ slug.text ++ ")",
      ..
    }

makeWikiLinkCompletionsFromLine :: NoteInfoCache -> Position -> Text -> [Completion]
makeWikiLinkCompletionsFromLine noteInfos position line = withEarlyReturn_ do
  let (rest, completionContext) = breakOnEnd "[[" line
  when ("]]" `isInfixOf` completionContext) $ returnEarly []
  when (null rest) $ returnEarly [] -- there is no `[[` in the line
  let (start, end) = breakOn "|" completionContext
      searchPrefix = if null end then start else end
  pure
    $ Fuzzy.filter searchPrefix (toList noteInfos) "" "" (.title) False
    <&> \(Fuzzy NoteInfo {..} _ _) ->
      WikiLinkCompletion
        { replaceRange = Range (sameLineWithCol position (length rest)) position,
          slug,
          title,
          label = title ++ " (" ++ slug.text ++ ")",
          typedAlias = completionContext
        }

renderCompletionForLineNum :: Completion -> [CompletionItem]
renderCompletionForLineNum completion@WikiLinkCompletion {..} =
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
          _detail = Nothing,
          _documentation = Nothing,
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
          _data_ = Just (toJSON completion)
       }

extractOriginalCompletion :: CompletionItem -> Either Text Completion
extractOriginalCompletion = 
  fromJSON <=< (maybe (Left "missing data value") Right . (._data_))

spec_makeWikiLinkCompletionsFromLine :: Spec
spec_makeWikiLinkCompletionsFromLine = do
  it "returns completions when there is a fuzzy match"
    $ makeWikiLinkCompletionsFromLine fakeNoteInfoCache (P 0 7) "[[Hello"
    `shouldBe` [wikiLinkCompletion (Range (P 0 2) (P 0 7)) (Slug "kWp7rk0suUXd") "Hello world" "Hello"]
  it "doesn't return completions when no [["
    $ makeWikiLinkCompletionsFromLine fakeNoteInfoCache (P 0 5) "Hello"
    `shouldBe` []
  it "returns all matching completions sorted in a reasonable order"
    $ makeWikiLinkCompletionsFromLine fakeNoteInfoCache (P 0 7) "[[world"
    `shouldBe` [ wikiLinkCompletion (Range (P 0 2) (P 0 7)) (Slug "7Fu2PSiqrvz4") "Test world" "world",
                 wikiLinkCompletion (Range (P 0 2) (P 0 7)) (Slug "kWp7rk0suUXd") "Hello world" "world",
                 wikiLinkCompletion (Range (P 0 2) (P 0 7)) (Slug "acZlsJzsFs2g") "A wild unordinary herald" "world"
               ]
  it "doesn't complete when there is a complete wiki link earlier in the line but no [["
    $ makeWikiLinkCompletionsFromLine fakeNoteInfoCache (P 0 28) "[[7Fu2PSiqrvz4|Hello]] world"
    `shouldBe` []
  it "is not confused by a second wikilink on the same line"
    $ makeWikiLinkCompletionsFromLine fakeNoteInfoCache (P 0 30) "[[7Fu2PSiqrvz4|Hello]] [[Hello"
    `shouldBe` [wikiLinkCompletion (Range (P 0 25) (P 0 30)) (Slug "kWp7rk0suUXd") "Hello world" "Hello"]
