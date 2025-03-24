{-# LANGUAGE QuasiQuotes #-}

module Models.Page.Utils
  ( -- * get various bits and bobs out of a page
    getTitle,
    getFirstH1Position,

    -- * utils
    attrB,
    attrI,
    attrRanges,

    -- * tests
    spec_pSourceRange,
    spec_getTitle,
    spec_getFirstH1Position,
  )
where

import Commonmark.Types (SourcePos, SourceRange (..))
import Control.Monad.State (get, put)
import Control.Monad.Trans.State (State, evalState)
import Language.LSP.Protocol.Lens qualified as J
import Language.LSP.Protocol.Types (Position (Position), Range (Range))
import Models.Page.TH
import MyPrelude
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as P
import Text.Pandoc.Definition
import Text.Parsec.Pos qualified as Parsec
import Utils.RangePosition

type Parser = P.ParsecT Void Text (State String)

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
      it (show x <> " fails to parse")
        $ runParser x
        `shouldSatisfy` has _Left
    x `parsesTo` y =
      it (show x <> " parses as " <> show (SourceRange y))
        $ runParser x
        `shouldBe` Right (SourceRange y)
    parseShowRoundtrips x =
      it (show x <> " parsed then shown is itself")
        $ (tshow <$> runParser x)
        `shouldBe` Right x

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
  it "gets the title from the heading"
    $ getTitle [md|# I am a title!|]
    `shouldBe` Just "I am a title!"
  it "it fails to get the title if there isn't a h1"
    $ getTitle [md|There is no title here|]
    `shouldBe` Nothing
  it "takes the first h1 even if there is stuff before it"
    $ getTitle
      [md|
        foo bar

        ## subheading before heading for whatever reason

        # A title
      |]
    `shouldBe` Just "A title"
  it "doesn't get confused by several instances of h1"
    $ getTitle
      [md|
        # A title

        # Another heading

        # A third heading
      |]
    `shouldBe` Just "A title"
  it "ignores comments"
    $
    -- this behavior is important for properly handling titles that themselves
    -- have a link to another note in them besides generally being sensible
    -- behavior
    getTitle
      [md|
        # A title with<!--a comment--> in it
        Body content
      |]
    `shouldBe` Just "A title with in it"

getFirstH1Position :: Pandoc -> Maybe Position
getFirstH1Position (Pandoc _ body) =
  foldMapA
    ( preview
        $ #_Header
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
  it "gets the title from the heading"
    $ getFirstH1Position [md|# I am a title!|]
    `shouldBe` Just (posAtLineCol 0 0)
  it "it fails to get the title if there isn't a h1"
    $ getFirstH1Position [md|There is no title here|]
    `shouldBe` Nothing
  it "takes the first h1 even if there is stuff before it"
    $ getFirstH1Position
      [md|
        foo bar

        ## subheading before heading for whatever reason

        # A title
      |]
    `shouldBe` Just (posAtLineCol 4 0)
  it "doesn't get confused by several instances of h1"
    $ getFirstH1Position
      [md|
        # A title

        # Another heading

        # A third heading
      |]
    `shouldBe` Just (posAtLineCol 0 0)
  it "isn't confused by yaml front matter"
    $ getFirstH1Position
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
