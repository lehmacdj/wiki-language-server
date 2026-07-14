module Models.Completion where

import Data.Aeson qualified as Aeson
import Data.Char (isAlphaNum)
import Data.IxSet.Typed qualified as IxSet
import Data.Map.Strict qualified as Map
import Data.Text (breakOn, breakOnEnd)
import Data.Text qualified as Text
import Language.LSP.Protocol.Types
import Models.NoteCreation qualified as NoteCreation
import Models.NoteInfo
import Models.Slug
import MyPrelude
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
  | CreateNoteCompletion
  { replaceRange :: Range,
    slug :: Slug,
    title :: Text,
    titleMarkdown :: Text
  }
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via FastGenericEncoding Completion

wikiLinkCompletion :: Range -> Slug -> Text -> Text -> Completion
wikiLinkCompletion replaceRange slug title typedAlias =
  WikiLinkCompletion
    { label = title ++ " (" ++ slug.text ++ ")",
      ..
    }

makeWikiLinkCompletionsFromLine ::
  NoteInfoCache -> Maybe Slug -> Position -> Text -> [Completion]
makeWikiLinkCompletionsFromLine noteInfos createSlug position line = withEarlyReturn_ do
  let (rest, completionContext) = breakOnEnd "[[" line
  when ("]]" `isInfixOf` completionContext) $ returnEarly []
  when (null rest) $ returnEarly [] -- there is no `[[` in the line
  let (start, end) = breakOn "|" completionContext
      searchPrefix = if null end then start else end
  -- completions aren't useful when the searchPrefix is empty
  when (null searchPrefix) $ returnEarly []
  let range = Range (sameLineWithCol position (length rest)) position
      toCompletion NoteInfo {..} =
        WikiLinkCompletion
          { replaceRange = range,
            slug,
            title,
            label = title ++ " (" ++ slug.text ++ ")",
            typedAlias = completionContext
          }
      fuzzyNotes = fuzzyMatchByTitle searchPrefix noteInfos
      fuzzyMatches = map toCompletion fuzzyNotes
      (strongFuzzy, weak) =
        partition (isStrongMatch searchPrefix . (.title)) fuzzyMatches
      fuzzySlugs = map (.slug) fuzzyNotes
      additionalStrong =
        IxSet.toList noteInfos
          & filter (isStrongMatch searchPrefix . (.title))
          & filter ((`notElem` fuzzySlugs) . (.slug))
          & sortOn (.title)
          & map toCompletion
      strong = strongFuzzy <> additionalStrong
      exactExists =
        any (titlesEqual searchPrefix . (.title)) $ IxSet.toList noteInfos
      create = do
        slug <- toList createSlug
        guard $ not exactExists
        draft <- toList $ either (const Nothing) Just $ NoteCreation.inferNoteDraft searchPrefix
        pure $
          CreateNoteCompletion
            { replaceRange = range,
              slug,
              title = draft.title,
              titleMarkdown = draft.titleMarkdown
            }
      createIsPremature =
        queryEndsWithIncompleteWord searchPrefix $ map (.title) fuzzyNotes
  pure $
    if createIsPremature
      then strong <> weak <> create
      else strong <> create <> weak

titlesEqual :: Text -> Text -> Bool
titlesEqual a b = Text.toCaseFold a == Text.toCaseFold b

isStrongMatch :: Text -> Text -> Bool
isStrongMatch query candidate =
  foldedQuery `Text.isInfixOf` foldedCandidate
    || queryWords `isSubmultisetOf` candidateWords
  where
    foldedQuery = Text.toCaseFold query
    foldedCandidate = Text.toCaseFold candidate
    queryWords = wordCounts foldedQuery
    candidateWords = wordCounts foldedCandidate
    wordCounts = Map.fromListWith (+) . map (,1 :: Int) . Text.words . Text.map wordChar
    wordChar c
      | isAlphaNum c = c
      | otherwise = ' '
    small `isSubmultisetOf` large =
      all (\(word, count) -> Map.findWithDefault 0 word large >= count) $ Map.toList small

queryEndsWithIncompleteWord :: Text -> [Text] -> Bool
queryEndsWithIncompleteWord query candidates =
  case (Text.unsnoc query, reverse $ normalizedWords query) of
    (Just (_, finalChar), queryWord : _)
      | isAlphaNum finalChar ->
          not (queryWord `elem` candidateWords)
            && any (isProperPrefix queryWord) candidateWords
    _ -> False
  where
    candidateWords = normalizedWords =<< candidates
    isProperPrefix prefix word =
      prefix /= word && prefix `Text.isPrefixOf` word
    normalizedWords =
      Text.words . Text.map wordChar . Text.toCaseFold
    wordChar c
      | isAlphaNum c = c
      | otherwise = ' '

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
                { _detail = Nothing,
                  _description = Just slug.text
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
renderCompletionForLineNum completion@CreateNoteCompletion {..} =
  [ CompletionItem
      { _label = "Create note \x201c" <> truncateWithMarker 45 "…" title <> "\x201d",
        _labelDetails = Nothing,
        _kind = Just CompletionItemKind_Constructor,
        _tags = Nothing,
        _detail = Just "Create a new wiki note",
        _documentation = Nothing,
        _deprecated = Nothing,
        _preselect = Just False,
        _sortText = Nothing,
        _filterText = Just titleMarkdown,
        _insertText = Nothing,
        _insertTextFormat = Nothing,
        _insertTextMode = Just InsertTextMode_AsIs,
        _textEdit = Just . InL . TextEdit replaceRange $
          slug.text <> "|" <> title <> "]]<!--wls-->",
        _textEditText = Nothing,
        _additionalTextEdits = Nothing,
        _commitCharacters = Nothing,
        _command =
          Just $
            Command
              "Create wiki note"
              "wiki.createNoteFromSelection"
              ( Just
                  [ Aeson.object
                      [ "completionTitle" Aeson..= titleMarkdown,
                        "slug" Aeson..= slug
                      ]
                  ]
              ),
        _data_ = Just (toJSON completion)
      }
  ]

extractOriginalCompletion :: CompletionItem -> Either Text Completion
extractOriginalCompletion =
  fromJSON <=< (maybe (Left "missing data value") Right . (._data_))

spec_makeWikiLinkCompletionsFromLine :: Spec
spec_makeWikiLinkCompletionsFromLine = do
  it "returns completions when there is a fuzzy match" $
    makeWikiLinkCompletionsFromLine fakeNoteInfoCache Nothing (P 0 7) "[[Hello"
      `shouldBe` [wikiLinkCompletion (Range (P 0 2) (P 0 7)) (Slug "kWp7rk0suUXd") "Hello world" "Hello"]
  it "doesn't return completions when no [[" $
    makeWikiLinkCompletionsFromLine fakeNoteInfoCache Nothing (P 0 5) "Hello"
      `shouldBe` []
  it "returns all matching completions sorted in a reasonable order" $
    makeWikiLinkCompletionsFromLine fakeNoteInfoCache Nothing (P 0 7) "[[world"
      `shouldBe` [ wikiLinkCompletion (Range (P 0 2) (P 0 7)) (Slug "7Fu2PSiqrvz4") "Test world" "world",
                   wikiLinkCompletion (Range (P 0 2) (P 0 7)) (Slug "kWp7rk0suUXd") "Hello world" "world",
                   wikiLinkCompletion (Range (P 0 2) (P 0 7)) (Slug "acZlsJzsFs2g") "A wild unordinary herald" "world"
                 ]
  it "doesn't complete when there is a complete wiki link earlier in the line but no [[" $
    makeWikiLinkCompletionsFromLine fakeNoteInfoCache Nothing (P 0 28) "[[7Fu2PSiqrvz4|Hello]] world"
      `shouldBe` []
  it "is not confused by a second wikilink on the same line" $
    makeWikiLinkCompletionsFromLine fakeNoteInfoCache Nothing (P 0 30) "[[7Fu2PSiqrvz4|Hello]] [[Hello"
      `shouldBe` [wikiLinkCompletion (Range (P 0 25) (P 0 30)) (Slug "kWp7rk0suUXd") "Hello world" "Hello"]
  it "places create between strong and weak fuzzy matches" do
    let result =
          makeWikiLinkCompletionsFromLine
            fakeNoteInfoCache
            (Just $ Slug "new-note")
            (P 0 7)
            "[[world"
    map completionName result
      `shouldBe` [
        "Test world",
        "Hello world",
        "Create world",
        "A wild unordinary herald"
      ]
  it "places create below fuzzy matches for an incomplete word" do
    let result =
          makeWikiLinkCompletionsFromLine
            fakeNoteInfoCache
            (Just $ Slug "new-note")
            (P 0 6)
            "[[worl"
    map completionName result
      `shouldBe` [
        "Test world",
        "Hello world",
        "A wild unordinary herald",
        "Create worl"
      ]
  it "treats whole query words in any order as strong" do
    let result =
          makeWikiLinkCompletionsFromLine
            fakeNoteInfoCache
            (Just $ Slug "new-note")
            (P 0 14)
            "[[herald wild"
    map completionName result
      `shouldBe` ["A wild unordinary herald", "Create herald wild"]
  it "does not offer create for a case-insensitive exact title" do
    let result =
          makeWikiLinkCompletionsFromLine
            fakeNoteInfoCache
            (Just $ Slug "new-note")
            (P 0 13)
            "[[HELLO WORLD"
    map completionName result `shouldBe` ["Hello world"]
  where
    completionName WikiLinkCompletion {title} = title
    completionName CreateNoteCompletion {titleMarkdown} = "Create " <> titleMarkdown
