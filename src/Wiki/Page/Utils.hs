{-# LANGUAGE QuasiQuotes #-}

module Wiki.Page.Utils
  ( -- * get various bits and bobs out of a page
    getTitle,

    -- * utils
    attrB,
    attrI,
    attrRanges,

    -- * tests
    spec_pSourceRange,
    spec_getTitle,
  )
where

import Commonmark.Types (SourcePos, SourceRange (..))
import Language.LSP.Types hiding (Empty)
import MyPrelude
import TestPrelude
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as P
import Text.Pandoc.Definition
import Text.Parsec.Pos qualified as Parsec
import Wiki.Page.TH

type Parser = P.ParsecT Void Text (State String)

-- | Affine traversal returning the attribute if this element has one, and
-- nothing otherwise
attrB :: Traversal' Block Attr
attrB f = \case
  -- note: we're intentionally not matching with {} here because we would want
  -- breakage if a new field is introduced because it could be a new Attr field
  p@(Plain _) -> pure p
  p@(Para _) -> pure p
  lb@(LineBlock _) -> pure lb
  CodeBlock attr t -> CodeBlock <$> f attr <*> pure t
  rb@(RawBlock _ _) -> pure rb
  bq@(BlockQuote _) -> pure bq
  ol@(OrderedList _ _) -> pure ol
  bl@(BulletList _) -> pure bl
  dl@(DefinitionList _) -> pure dl
  Header l attr b -> Header l <$> f attr <*> pure b
  HorizontalRule -> pure HorizontalRule
  Table attr c cs th tb tf ->
    Table <$> f attr <*> pure c <*> pure cs <*> pure th <*> pure tb <*> pure tf
  Div attr b -> Div <$> f attr <*> pure b
  Null -> pure Null

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

-- performance note: if this turns out to be performance critical, an option
-- would be to not use the pandoc ast and instead have a custom ast that doesn't
-- convert the 'SourceRange' into a string when inserting it into the AST
parseRange :: Text -> Maybe SourceRange
parseRange =
  either (const Nothing) Just
    . (`evalState` "")
    . P.runParserT (pSourceRange <* P.eof) "<internal>"

pPos :: Parser SourcePos
pPos = Parsec.newPos <$> get <*> (P.decimal <* P.char ':') <*> P.decimal

pSourceName :: Parser String
pSourceName = P.manyTill P.anySingle "@"

tryUpdateSourceName :: Parser ()
tryUpdateSourceName = traverse_ put =<< P.option Nothing (Just <$> P.try pSourceName)

pRange :: Parser (SourcePos, SourcePos)
pRange = do
  tryUpdateSourceName
  p1 <- pPos
  -- in fairly degenerate situations it is theoretically possible for the
  -- start and end of the range to be in distinct files. See the show
  -- instance for 'SourceRange'
  _ <- P.char '-'
  tryUpdateSourceName
  p2 <- pPos
  pure (p1, p2)

pSourceRange :: Parser SourceRange
pSourceRange = SourceRange <$> pRange `P.sepBy` P.char ';'

spec_pSourceRange :: Spec
spec_pSourceRange = do
  "0:0-1:1" `parsesTo` [(Parsec.newPos "" 0 0, Parsec.newPos "" 1 1)]
  -- this doesn't parse due to degeneracy in the leniency for parsing the
  -- filename. See this issue for details:
  -- https://github.com/jgm/commonmark-hs/issues/91
  -- "0:0-@1:1" `parsesTo` [(Parsec.newPos "" 0 0, Parsec.newPos "" 1 1)]
  "@1:1-2:2" `parsesTo` [(Parsec.newPos "" 1 1, Parsec.newPos "" 2 2)]
  "asdf@1:1-@2:2" `parsesTo` [(Parsec.newPos "asdf" 1 1, Parsec.newPos "" 2 2)]
  "asdf@0:0-jkl@1:1"
    `parsesTo` [(Parsec.newPos "asdf" 0 0, Parsec.newPos "jkl" 1 1)]
  "0:0-1:1;2:2-3:3"
    `parsesTo` [ (Parsec.newPos "" 0 0, Parsec.newPos "" 1 1),
                 (Parsec.newPos "" 2 2, Parsec.newPos "" 3 3)
               ]
  "a@0:0-1:1;2:2-3:3"
    `parsesTo` [ (Parsec.newPos "a" 0 0, Parsec.newPos "a" 1 1),
                 (Parsec.newPos "a" 2 2, Parsec.newPos "a" 3 3)
               ]
  parseShowRoundtrips "0:0-1:1"
  parseShowRoundtrips "asdf@0:0-@1:1"
  -- this example is fairly degenerate: see this issue for details
  -- https://github.com/jgm/commonmark-hs/issues/91
  parseShowRoundtrips "asdf@0:0-1:1;2:2-@3:3"
  parseShowRoundtrips "a@0:0-1:12;3:3-asdf@4:4"
  failsToParse "asdf"
  failsToParse "asdf@"
  failsToParse "asdf@asdf@"
  failsToParse ":"
  failsToParse ";"
  failsToParse "-"
  failsToParse ":1-1:1"
  where
    runParser x =
      let r = evalState (P.runParserT (pSourceRange <* P.eof) "<test>" x) ""
       in left P.errorBundlePretty r
    failsToParse x =
      it (show x <> " fails to parse") $
        runParser x `shouldSatisfy` has _Left
    x `parsesTo` y =
      it (show x <> " parses as " <> show (SourceRange y)) $
        runParser x `shouldBe` Right (SourceRange y)
    parseShowRoundtrips x =
      it (show x <> " parsed then shown is itself") $
        (tshow <$> runParser x) `shouldBe` Right x

sourcePosPairToRange :: (SourcePos, SourcePos) -> Range
sourcePosPairToRange (p1, p2) =
  Range
    -- this needs to adapt parsec's 1 based indexing for LSP protocols 0 based
    -- indexing
    (Position (fromIntegral (Parsec.sourceLine p1) - 1) (fromIntegral (Parsec.sourceColumn p1) - 1))
    (Position (fromIntegral (Parsec.sourceLine p2) - 1) (fromIntegral (Parsec.sourceColumn p2) - 1))

attrRanges :: Attr -> Maybe [Range]
attrRanges (_id, _classes, kvs) = case lookup "data-pos" kvs of
  Just (parseRange -> Just (SourceRange ranges)) ->
    Just $ map sourcePosPairToRange ranges
  Just _ -> Nothing
  Nothing -> Nothing

-- We define the title of a document as the @title@ attribute in a yaml
-- frontmatter, if that doesn't exist the first heading, and if that doesn't
-- exist @Nothing@.
--
-- There might be room in the future to extend this to be smarter. For example
-- we could further fall back to the first sentence in the document etc.
getTitle :: Pandoc -> Maybe Text
getTitle (Pandoc (Meta meta) body) =
  meta ^? ix "title" . #_MetaString
    <|> firstHeading body
  where
    firstHeading =
      foldMapA . preview $
        #_Header . filteredBy (_1 . only 1) . _3 . to titleSanitizeInline
    -- this tries to do fairly reasonable things, but isn't very precisely
    -- defined or well specified
    titleSanitizeInline = foldMap \case
      Cite _ rec -> titleSanitizeInline rec
      Code _ t -> t
      Emph rec -> titleSanitizeInline rec
      Image _ c _ -> titleSanitizeInline c
      LineBreak -> " " -- titles don't want to be multiple lines even if the h1 is
      Link _ rec _ -> titleSanitizeInline rec
      Math _ t -> t
      Note _ -> ""
      Quoted SingleQuote rec -> "'" <> titleSanitizeInline rec <> "'"
      Quoted DoubleQuote rec -> "\"" <> titleSanitizeInline rec <> "\""
      -- ignore HTML comments, slightly broken in case of weird nested comment
      -- situations, but HTML in titles should be rare, and weird edge behavior
      -- is okay for it
      RawInline _ t | "<!--" `isPrefixOf` t && "-->" `isSuffixOf` t -> ""
      RawInline _ t -> t
      SmallCaps rec -> titleSanitizeInline rec
      SoftBreak -> " "
      Space -> " "
      Span _ rec -> titleSanitizeInline rec
      Str t -> t
      Strikeout rec -> titleSanitizeInline rec
      Strong rec -> titleSanitizeInline rec
      Subscript rec -> titleSanitizeInline rec
      Superscript rec -> titleSanitizeInline rec
      Underline rec -> titleSanitizeInline rec

spec_getTitle :: Spec
spec_getTitle = do
  -- TODO: once yaml frontmatter parsing is implemented, add tests for
  -- frontmatter component of logic
  it "gets the title from the heading" $
    getTitle [md|# I am a title!|] `shouldBe` Just "I am a title!"
  it "it fails to get the title if there isn't a h1" $
    getTitle [md|There is no title here|] `shouldBe` Nothing
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
