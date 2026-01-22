module Models.Page.Parser where

import Commonmark hiding (BulletList, OrderedList)
import Commonmark.Extensions
import Commonmark.Pandoc
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Yaml qualified as Yaml
import Models.Diagnostics
import Models.Page.SourcePos (offsetAttr)
import MyPrelude
import Text.Pandoc.Builder qualified as Pandoc
import Text.Pandoc.Definition
import Text.Pandoc.Walk (walk)
import Text.Parsec.Error qualified as Parsec
import Text.Parsec.Pos qualified as Parsec
import Utils.RangePosition
import Utils.TH

parseErrorFromParsec :: Parsec.ParseError -> Diagnostic
parseErrorFromParsec err =
  mkDiagnostic ParseError (atLineCol line col) (pretty (tshow err))
  where
    line = Parsec.sourceLine $ Parsec.errorPos err
    col = Parsec.sourceColumn $ Parsec.errorPos err

toPandoc :: Cm SourceRange Pandoc.Blocks -> Pandoc
toPandoc = Pandoc.doc . unCm

syntaxSpec ::
  (Monad m, Typeable m) =>
  SyntaxSpec m (Cm SourceRange Pandoc.Inlines) (Cm SourceRange Pandoc.Blocks)
syntaxSpec =
  defaultSyntaxSpec
    <> gfmExtensions
    <> wikilinksSpec TitleAfterPipe

-- | Extract YAML frontmatter from text. Returns (Maybe frontmatter, body, lineOffset).
-- Frontmatter must start with "---" on the first line and end with "---".
-- The lineOffset is the number of lines the frontmatter occupies (including delimiters).
extractFrontmatter :: Text -> (Maybe Text, Text, Int)
extractFrontmatter t =
  case lines t of
    ("---" : rest) ->
      case break (== "---") rest of
        (yaml, "---" : body) ->
          -- +2 for the two "---" lines
          (Just (unlines yaml), unlines body, length yaml + 2)
        _ -> (Nothing, t, 0)
    _ -> (Nothing, t, 0)

-- | Convert a YAML Value to a Pandoc MetaValue
yamlToMeta :: Yaml.Value -> MetaValue
yamlToMeta = \case
  Yaml.String s -> MetaString s
  Yaml.Bool b -> MetaBool b
  Yaml.Number n -> MetaString (tshow n)
  Yaml.Array arr -> MetaList $ map yamlToMeta (toList arr)
  Yaml.Object obj ->
    MetaMap . mapFromList $
      [(Key.toText k, yamlToMeta v) | (k, v) <- KeyMap.toList obj]
  Yaml.Null -> MetaString ""

-- | Parse YAML frontmatter into Pandoc Meta
parseFrontmatter :: Text -> Either Diagnostic Meta
parseFrontmatter yaml =
  case Yaml.decodeEither' (encodeUtf8 yaml) of
    Left err -> Left $ mkDiagnostic ParseError pos (pretty (tshow err))
    Right val -> case yamlToMeta val of
      MetaMap m -> Right $ Meta m
      _ -> Left $ mkDiagnostic ParseError pos "Frontmatter must be a YAML object"
  where
    pos = atLineCol 1 1

-- | Offset all source positions in a Block by the given line offset.
-- Only blocks with Attr contain position information that needs offsetting.
offsetBlockPositions :: Int -> Block -> Block
offsetBlockPositions offset = \case
  CodeBlock attr t -> CodeBlock (offsetAttr offset attr) t
  Header n attr inlines -> Header n (offsetAttr offset attr) inlines
  Table attr caption colSpecs thead tbodies tfoot ->
    Table (offsetAttr offset attr) caption colSpecs thead tbodies tfoot
  Div attr blocks -> Div (offsetAttr offset attr) blocks
  Figure attr caption blocks -> Figure (offsetAttr offset attr) caption blocks
  block -> block

-- | Offset all source positions in an Inline by the given line offset.
-- Only inlines with Attr contain position information that needs offsetting.
offsetInlinePositions :: Int -> Inline -> Inline
offsetInlinePositions offset = \case
  Code attr t -> Code (offsetAttr offset attr) t
  Link attr inlines target -> Link (offsetAttr offset attr) inlines target
  Image attr inlines target -> Image (offsetAttr offset attr) inlines target
  Span attr inlines -> Span (offsetAttr offset attr) inlines
  inline -> inline

-- | Offset all source positions in a Pandoc document by the given line offset.
offsetPositions :: Int -> Pandoc -> Pandoc
offsetPositions offset (Pandoc meta blocks) =
  Pandoc meta (walk (offsetInlinePositions offset) (walk (offsetBlockPositions offset) blocks))

parse :: FilePath -> Text -> Either Diagnostic Pandoc
parse filepath t = do
  let (mFrontmatter, body, lineOffset) = extractFrontmatter t
  meta <- maybe (Right mempty) parseFrontmatter mFrontmatter
  r <- commonmarkWith syntaxSpec filepath body
  case r of
    Left err -> Left $ parseErrorFromParsec err
    Right v ->
      let Pandoc _ blocks = toPandoc v
          doc = Pandoc meta blocks
       in Right $ applyOffset lineOffset doc
  where
    applyOffset n doc
      | n > 0 = offsetPositions n doc
      | otherwise = doc

test_parse :: [TestTree]
test_parse =
  [ goldenParse
      "simple"
      [rTrim|
      # Hello World
      This is a simple *Markdown* document.
    |],
    goldenParse
      "frontmatter"
      [rTrim|
      ---
      title: This has a frontmatter
      tags:
        - foo
        - bar
      ---

      Some text
      |]
  ]
  where
    goldenParse :: (HasCallStack) => String -> Text -> TestTree
    goldenParse name input = goldenTestShow name $ parse "test.md" input
