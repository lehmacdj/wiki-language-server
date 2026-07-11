{-# LANGUAGE QuasiQuotes #-}

module Models.NoteCreation
  ( NoteDraft (..),
    inferNoteDraft,
    renderNote,
    replacementLink,
    isStub,
    spec_inferNoteDraft,
    spec_renderNote,
  )
where

import Data.Char (isDigit, isSpace)
import Data.Text qualified as Text
import Models.Page.Parser qualified as Page
import Models.Page.TH
import Models.Page.Utils qualified as Page
import Models.Slug (Slug (..))
import MyPrelude
import Text.Pandoc.Definition

data NoteDraft = NoteDraft
  { titleMarkdown :: Text,
    title :: Text,
    body :: Maybe Text,
    publicAlternate :: Maybe Text,
    sourcePrefix :: Text
  }
  deriving (Show, Eq)

-- | Infer a note from exactly the selected markdown. Outer whitespace is
-- ignored, but content outside the selection is never inspected.
inferNoteDraft :: Text -> Either Text NoteDraft
inferNoteDraft selected = do
  let input = Text.strip selected
  when (null input) $ Left "Cannot create a note from an empty selection"
  case parseStructure input of
    Just (prefix, titleSource, structuralBody) ->
      finish prefix titleSource structuralBody
    Nothing
      | '\n' `elem` input ->
          Left $
            "Cannot infer a title from multiline content; select one heading "
              <> "or one root list item"
      | otherwise -> finish "" input Nothing
  where
    finish prefix titleSource structuralBody = do
      let (titleMarkdown, publicAlternate, leadingBody) =
            case parseLeadingLink titleSource of
              Just (label, url, remainder) ->
                (label, Just url, nonEmpty $ trimSeparators remainder)
              Nothing -> (Text.strip titleSource, Nothing, Nothing)
          body = joinBody leadingBody structuralBody
      title <- sanitizedTitle titleMarkdown
      when (null title) $ Left "The inferred note title is empty"
      pure NoteDraft {sourcePrefix = prefix, ..}

parseStructure :: Text -> Maybe (Text, Text, Maybe Text)
parseStructure input = parseHeading input <|> parseListItem input

parseHeading :: Text -> Maybe (Text, Text, Maybe Text)
parseHeading input = do
  let (firstLine, rest) = break (== '\n') input
      (hashes, afterHashes) = span (== '#') firstLine
      level = length hashes
  guard $ not (null hashes) && length hashes <= 6
  title <- stripPrefix " " afterHashes
  let body = Text.stripStart $ drop 1 rest
  guard $ all (not . isRootHeading level) $ lines body
  pure ("", title, nonEmpty body)
  where
    isRootHeading parentLevel line =
      let (hashes, suffix) = span (== '#') $ Text.stripStart line
       in not (null hashes)
            && length hashes <= parentLevel
            && " " `isPrefixOf` suffix

parseListItem :: Text -> Maybe (Text, Text, Maybe Text)
parseListItem input = do
  (firstLine, rest) <- uncons $ lines input
  let (indent, unindented) = span isSpace firstLine
  (marker, afterMarker) <- listMarker unindented
  let (task, title) = taskMarker afterMarker
      prefix = indent <> marker <> task
      childIndent = length indent + length marker
  guard $ all (isChildLine childIndent) rest
  let children =
        rest
          & map (dropAtMost childIndent)
          & unlines
          & Text.stripEnd
  pure (prefix, title, nonEmpty children)
  where
    isChildLine _ line | all isSpace line = True
    isChildLine amount line = length (takeWhile isSpace line) >= amount
    dropAtMost amount line = drop (min amount $ length $ takeWhile isSpace line) line

listMarker :: Text -> Maybe (Text, Text)
listMarker text =
  asum
    [ do
        marker <- listToMaybe ["- ", "* ", "+ "] >>= (`stripPrefix` text)
        let markerLength = length text - length marker
        pure (take markerLength text, marker),
      do
        let (digits, rest) = span isDigit text
        guard $ not (null digits)
        after <- stripPrefix ". " rest
        pure (digits <> ". ", after)
    ]

taskMarker :: Text -> (Text, Text)
taskMarker text =
  case find (`isPrefixOf` text) ["[ ] ", "[x] ", "[X] "] of
    Just marker -> (marker, drop (length marker) text)
    Nothing -> ("", text)

-- | Parse an external markdown link at the beginning of some text.
parseLeadingLink :: Text -> Maybe (Text, Text, Text)
parseLeadingLink text = do
  afterOpen <- stripPrefix "[" text
  let (label, afterLabel) = Text.breakOn "](" afterOpen
  guard $ not (null afterLabel)
  let urlAndRest = drop 2 afterLabel
      (url, rest) = balancedUrl 0 [] (unpack urlAndRest)
  remaining <- rest
  guard $ not (null label) && not (null url)
  pure (label, pack $ reverse url, pack remaining)

balancedUrl :: Int -> String -> String -> (String, Maybe String)
balancedUrl _ acc [] = (acc, Nothing)
balancedUrl depth acc ('(' : rest) = balancedUrl (depth + 1) ('(' : acc) rest
balancedUrl 0 acc (')' : rest) = (acc, Just rest)
balancedUrl depth acc (')' : rest) = balancedUrl (depth - 1) (')' : acc) rest
balancedUrl depth acc (c : rest) = balancedUrl depth (c : acc) rest

trimSeparators :: Text -> Text
trimSeparators =
  dropWhile (\c -> isSpace c || c `elem` (":-–—" :: String))

joinBody :: Maybe Text -> Maybe Text -> Maybe Text
joinBody Nothing Nothing = Nothing
joinBody (Just a) Nothing = Just a
joinBody Nothing (Just b) = Just b
joinBody (Just a) (Just b) = Just $ a <> "\n" <> b

nonEmpty :: Text -> Maybe Text
nonEmpty t
  | null t = Nothing
  | otherwise = Just t

sanitizedTitle :: Text -> Either Text Text
sanitizedTitle titleMarkdown = do
  parsed <-
    Page.parse "<note-title>" ("# " <> titleMarkdown)
      & first (view $ #_message . to tshow)
  Page.getTitle parsed `onNothing` Left "Could not extract the inferred title"

renderNote :: ZonedTime -> NoteDraft -> Text
renderNote now NoteDraft {titleMarkdown, body, publicAlternate} =
  let date = pack $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Ez" now
      alternate = maybe "" ("public_alternate: " <>) publicAlternate
      renderedBody = maybe "" ("\n" <>) body
   in case publicAlternate of
        Nothing ->
          [mdText|
            ---
            date: $(date)
            ---

            # $(titleMarkdown)$(renderedBody)
          |]
        Just _ ->
          [mdText|
            ---
            date: $(date)
            $(alternate)
            ---

            # $(titleMarkdown)$(renderedBody)
          |]

replacementLink :: Slug -> NoteDraft -> Text
replacementLink slug draft =
  draft.sourcePrefix
    <> "[["
    <> slug.text
    <> "|"
    <> draft.title
    <> "]]<!--wls-->"

isStub :: Pandoc -> Bool
isStub (Pandoc _ blocks) =
  case filter (not . ignorable) blocks of
    [Header 1 _ _] -> True
    _ -> False
  where
    ignorable = \case
      RawBlock (Format "html") _ -> True
      _ -> False

spec_inferNoteDraft :: Spec
spec_inferNoteDraft = do
  it "infers a plain title" $
    inferNoteDraft "Some title"
      `shouldBe` Right (NoteDraft "Some title" "Some title" Nothing Nothing "")
  it "preserves markdown in the H1 but sanitizes the transcluded title" $
    inferNoteDraft "**Important** `concept`"
      `shouldBe` Right
        (NoteDraft "**Important** `concept`" "Important concept" Nothing Nothing "")
  it "moves list children and preserves the prefix" $
    inferNoteDraft "- [ ] Parent\n  - Child"
      `shouldBe` Right (NoteDraft "Parent" "Parent" (Just "- Child") Nothing "- [ ] ")
  it "removes a heading from the replacement" $
    inferNoteDraft "## Parent\nBody"
      `shouldBe` Right (NoteDraft "Parent" "Parent" (Just "Body") Nothing "")
  it "extracts a leading link and separator" $
    inferNoteDraft "[Title](https://example.com) — description"
      `shouldBe` Right (NoteDraft "Title" "Title" (Just "description") (Just "https://example.com") "")
  it "composes list and link inference" $
    inferNoteDraft "- [Title](https://example.com): description\n  - Child"
      `shouldBe` Right (NoteDraft "Title" "Title" (Just "description\n- Child") (Just "https://example.com") "- ")
  it "rejects unmatched multiline text" $
    inferNoteDraft "one\ntwo" `shouldSatisfy` either (const True) (const False)
  it "rejects multiple root headings" $
    inferNoteDraft "## One\n## Two" `shouldSatisfy` either (const True) (const False)

spec_renderNote :: Spec
spec_renderNote = do
  it "renders a compact note with alternate URL" do
    let now = ZonedTime (LocalTime (fromGregorian 2026 7 11) midnight) utc
        draft = NoteDraft "Title" "Title" (Just "description") (Just "https://example.com") ""
    renderNote now draft
      `shouldBe` unlines
        [ "---",
          "date: 2026-07-11T00:00:00+00:00",
          "public_alternate: https://example.com",
          "---",
          "",
          "# Title",
          "description"
        ]
