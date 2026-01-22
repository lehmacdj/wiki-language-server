{-# LANGUAGE QuasiQuotes #-}

module Models.Page.Utils
  ( -- * get various bits and bobs out of a page
    getTitle,
    dayNoteTitleToDay,
    getFirstH1Position,

    -- * utils
    attrB,
    attrI,
    attrRanges,

    -- * tests
    spec_pSourceRange,
    spec_dayNoteTitleToDay,
    spec_getTitle,
    spec_getFirstH1Position,
  )
where

import Language.LSP.Protocol.Lens qualified as J
import Language.LSP.Protocol.Types (Position)
import Models.Page.SourcePos (attrRanges, spec_pSourceRange)
import Models.Page.TH
import MyPrelude
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Pandoc.Definition
import Utils.RangePosition

-- | Affine traversal returning the attribute if this element has one, and
-- nothing otherwise
attrB :: Traversal' Block Attr
attrB f = \case
  -- note: we're intentionally not matching with {} here because we would want
  -- breakage if a new field is introduced because it could be a new Attr field
  p@Plain {} -> pure p
  p@Para {} -> pure p
  lb@LineBlock {} -> pure lb
  CodeBlock attr t -> CodeBlock <$> f attr <*> pure t
  rb@RawBlock {} -> pure rb
  bq@BlockQuote {} -> pure bq
  ol@OrderedList {} -> pure ol
  bl@BulletList {} -> pure bl
  bl@Figure {} -> pure bl
  dl@DefinitionList {} -> pure dl
  Header l attr b -> Header l <$> f attr <*> pure b
  HorizontalRule -> pure HorizontalRule
  Table attr c cs th tb tf ->
    Table <$> f attr <*> pure c <*> pure cs <*> pure th <*> pure tb <*> pure tf
  Div attr b -> Div <$> f attr <*> pure b

-- | Affine traversal returning the attribute if this element has one, and
-- nothing otherwise
attrI :: Traversal' Inline Attr
attrI f = \case
  -- note: we're intentionally not matching with {} here because we would want
  -- breakage if a new field is introduced because it could be a new Attr field
  s@(Str _) -> pure s
  e@(Emph _) -> pure e
  u@(Underline _) -> pure u
  s@(Strong _) -> pure s
  s@(Strikeout _) -> pure s
  s@(Superscript _) -> pure s
  s@(Subscript _) -> pure s
  sc@(SmallCaps _) -> pure sc
  q@(Quoted _ _) -> pure q
  c@(Cite _ _) -> pure c
  Code attr t -> Code <$> f attr <*> pure t
  Space -> pure Space
  SoftBreak -> pure SoftBreak
  LineBreak -> pure LineBreak
  m@(Math _ _) -> pure m
  ri@(RawInline _ _) -> pure ri
  Link attr i t -> Link <$> f attr <*> pure i <*> pure t
  Image attr i t -> Image <$> f attr <*> pure i <*> pure t
  n@(Note _) -> pure n
  Span attr i -> Span <$> f attr <*> pure i

-- We define the title of a document as the @title@ attribute in a yaml
-- frontmatter, if that doesn't exist the first heading, and if that doesn't
-- exist @Nothing@.
--
-- There might be room in the future to extend this to be smarter. For example
-- we could further fall back to the first sentence in the document etc.
getTitle :: Pandoc -> Maybe Text
getTitle (Pandoc (Meta meta) body) =
  meta
    ^? ix "title"
      . #_MetaString
      <|> firstHeading body
  where
    firstHeading =
      foldMapA
        . preview
        $ #_Header
          . filteredBy (_1 . only 1)
          . _3
          . to sanitizeTitleInline

-- | Sanitize the title by removing any elements that are not text
sanitizeTitleInline :: [Inline] -> Text
sanitizeTitleInline = foldMap \case
  Cite _ rec -> sanitizeTitleInline rec
  Code _ t -> t
  Emph rec -> sanitizeTitleInline rec
  Image _ c _ -> sanitizeTitleInline c
  LineBreak -> " " -- titles don't want to be multiple lines even if the h1 is
  Link _ rec _ -> sanitizeTitleInline rec
  Math _ t -> t
  Note _ -> ""
  Quoted SingleQuote rec -> "'" <> sanitizeTitleInline rec <> "'"
  Quoted DoubleQuote rec -> "\"" <> sanitizeTitleInline rec <> "\""
  -- ignore HTML comments, slightly broken in case of weird nested comment
  -- situations, but HTML in titles should be rare, and weird edge behavior
  -- is okay for it
  RawInline _ t | "<!--" `isPrefixOf` t && "-->" `isSuffixOf` t -> ""
  RawInline _ t -> t
  SmallCaps rec -> sanitizeTitleInline rec
  SoftBreak -> " "
  Space -> " "
  Span _ rec -> sanitizeTitleInline rec
  Str t -> t
  Strikeout rec -> sanitizeTitleInline rec
  Strong rec -> sanitizeTitleInline rec
  Subscript rec -> sanitizeTitleInline rec
  Superscript rec -> sanitizeTitleInline rec
  Underline rec -> sanitizeTitleInline rec

spec_getTitle :: Spec
spec_getTitle = do
  -- TODO: once yaml frontmatter parsing is implemented, add tests for
  -- frontmatter component of logic
  it "gets the title from the heading" $
    getTitle [md|# I am a title!|]
      `shouldBe` Just "I am a title!"
  it "it fails to get the title if there isn't a h1" $
    getTitle [md|There is no title here|]
      `shouldBe` Nothing
  it "takes the first h1 even if there is stuff before it" $
    getTitle
      [md|
        foo bar

        ## subheading before heading for whatever reason

        # A title
      |]
      `shouldBe` Just "A title"
  it "doesn't get confused by several instances of h1" $
    getTitle
      [md|
        # A title

        # Another heading

        # A third heading
      |]
      `shouldBe` Just "A title"
  it "ignores comments" $
    -- this behavior is important for properly handling titles that themselves
    -- have a link to another note in them besides generally being sensible
    -- behavior
    getTitle
      [md|
        # A title with<!--a comment--> in it
        Body content
      |]
      `shouldBe` Just "A title with in it"

dayNoteTitleToDay :: Text -> Maybe Day
dayNoteTitleToDay title = do
  -- Parse a date from the start of the title
  (dateStr, rest) <- P.parseMaybe pDatePrefix title
  -- Reject if there's another date in the suffix
  guard $ not $ hasDatePattern rest
  parseTimeM True defaultTimeLocale "%Y-%m-%d" (unpack dateStr)
  where
    -- Parser that extracts the date prefix and remaining text
    pDatePrefix :: P.Parsec Void Text (Text, Text)
    pDatePrefix = do
      dateStr <- P.takeP (Just "date") 10
      rest <- P.takeRest
      pure (dateStr, rest)
    -- Check if text contains a date pattern (YYYY-MM-DD)
    hasDatePattern :: Text -> Bool
    hasDatePattern t = isJust $ P.parseMaybe pFindDate t
    pFindDate :: P.Parsec Void Text ()
    pFindDate = void $ P.manyTill P.anySingle pDatePattern
    pDatePattern :: P.Parsec Void Text ()
    pDatePattern = void $ do
      _ <- P.count 4 P.digitChar
      _ <- P.char '-'
      _ <- P.count 2 P.digitChar
      _ <- P.char '-'
      P.count 2 P.digitChar

spec_dayNoteTitleToDay :: Spec
spec_dayNoteTitleToDay = do
  let getsDayFromValidTitle title day =
        it (unpack title) $ dayNoteTitleToDay title `shouldBe` Just day
      failsToParse title =
        it ("fails to parse " ++ show title) $
          dayNoteTitleToDay title `shouldBe` Nothing
  "2023-08-15" `getsDayFromValidTitle` fromGregorian 2023 8 15
  "2023-08-15 (parenthesized text)"
    `getsDayFromValidTitle` fromGregorian 2023 8 15
  "2023-08-15 - textual description"
    `getsDayFromValidTitle` fromGregorian 2023 8 15
  it "fails to parse an invalid date title" $
    dayNoteTitleToDay "not-a-date"
      `shouldBe` Nothing
  describe "date needs to come first" do
    failsToParse "some description 2023-08-15"
    failsToParse "some description - 2023-08-15"
    failsToParse "some description (2023-08-15)"
  describe "fails with multiple dates" do
    failsToParse "2023-08-15 and also 2023-08-16"
    failsToParse "2023-08-15 - 2023-08-16"
    failsToParse "2023-08-15 through 2023-08-16"

getFirstH1Position :: Pandoc -> Maybe Position
getFirstH1Position (Pandoc _ body) =
  foldMapA
    ( preview $
        #_Header
          . filteredBy (_1 . only 1)
          . _2
          . to attrRanges
          . _Just
          . _head
          . J.start
          . to (`sameLineWithCol` 0)
    )
    body

spec_getFirstH1Position :: Spec
spec_getFirstH1Position = do
  it "gets the title from the heading" $
    getFirstH1Position [md|# I am a title!|]
      `shouldBe` Just (posAtLineCol 0 0)
  it "it fails to get the title if there isn't a h1" $
    getFirstH1Position [md|There is no title here|]
      `shouldBe` Nothing
  it "takes the first h1 even if there is stuff before it" $
    getFirstH1Position
      [md|
        foo bar

        ## subheading before heading for whatever reason

        # A title
      |]
      `shouldBe` Just (posAtLineCol 4 0)
  it "doesn't get confused by several instances of h1" $
    getFirstH1Position
      [md|
        # A title

        # Another heading

        # A third heading
      |]
      `shouldBe` Just (posAtLineCol 0 0)
  it "isn't confused by yaml front matter" $
    getFirstH1Position
      [md|
        ---
        tags:
        - foo
        - bar
        ---

        # A title with<!--a comment--> in it
        Body content
      |]
      `shouldBe` Just (posAtLineCol 6 0)
