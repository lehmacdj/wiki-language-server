module Models.Page.SourcePos
  ( -- * Parsing
    parseSourceRange,
    pSourceRange,

    -- * Conversion
    sourcePosPairToRange,
    attrRanges,

    -- * Offsetting
    offsetSourceRange,
    offsetDataPos,
    offsetAttr,

    -- * Tests
    spec_pSourceRange,
  )
where

import Commonmark.Types (SourcePos, SourceRange (..))
import Control.Monad.State (gets, put)
import Control.Monad.Trans.State (State, evalState)
import Language.LSP.Protocol.Types (Position (Position), Range (Range))
import MyPrelude
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as P
import Text.Pandoc.Definition (Attr)
import Text.Parsec.Pos qualified as Parsec

type Parser = P.ParsecT Void Text (State String)

-- | Parse a data-pos attribute value into a SourceRange.
parseSourceRange :: Text -> Maybe SourceRange
parseSourceRange =
  either (const Nothing) Just
    . (`evalState` "")
    . P.runParserT (pSourceRange <* P.eof) "<internal>"

pPos :: Parser SourcePos
pPos = gets Parsec.newPos <*> (P.decimal <* P.char ':') <*> P.decimal

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

-- | Convert a SourcePos pair to an LSP Range.
-- Adapts Parsec's 1-based indexing to LSP's 0-based indexing.
sourcePosPairToRange :: (SourcePos, SourcePos) -> Range
sourcePosPairToRange (p1, p2) = Range (toPosition p1) (toPosition p2)
  where
    toPosition p =
      Position
        (fromIntegral (Parsec.sourceLine p) - 1)
        (fromIntegral (Parsec.sourceColumn p) - 1)

-- | Extract LSP ranges from a Pandoc Attr's data-pos attribute.
attrRanges :: Attr -> Maybe [Range]
attrRanges (_id, _classes, kvs) = do
  dataPos <- lookup "data-pos" kvs
  SourceRange ranges <- parseSourceRange dataPos
  pure $ map sourcePosPairToRange ranges

-- | Offset a SourcePos by a number of lines.
offsetSourcePos :: Int -> SourcePos -> SourcePos
offsetSourcePos offset pos =
  Parsec.setSourceLine pos (Parsec.sourceLine pos + offset)

-- | Offset all positions in a SourceRange by a number of lines.
offsetSourceRange :: Int -> SourceRange -> SourceRange
offsetSourceRange offset (SourceRange ranges) =
  SourceRange [(offsetSourcePos offset p1, offsetSourcePos offset p2) | (p1, p2) <- ranges]

-- | Offset line numbers in a data-pos attribute value by the given amount.
-- Parses the value, applies the offset, and converts back to text.
offsetDataPos :: Int -> Text -> Text
offsetDataPos offset pos = case parseSourceRange pos of
  Just sr -> tshow (offsetSourceRange offset sr)
  Nothing -> pos

-- | Offset all data-pos attributes in Attr by the given line offset.
offsetAttr :: Int -> Attr -> Attr
offsetAttr offset (ident, classes, kvs) =
  (ident, classes, map offsetKV kvs)
  where
    offsetKV ("data-pos", v) = ("data-pos", offsetDataPos offset v)
    offsetKV kv = kv

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
        runParser x
          `shouldSatisfy` has _Left
    x `parsesTo` y =
      it (show x <> " parses as " <> show (SourceRange y)) $
        runParser x
          `shouldBe` Right (SourceRange y)
    parseShowRoundtrips x =
      it (show x <> " parsed then shown is itself") $
        (tshow <$> runParser x)
          `shouldBe` Right x
