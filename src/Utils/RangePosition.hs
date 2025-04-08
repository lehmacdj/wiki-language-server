module Utils.RangePosition where

import Language.LSP.Protocol.Types
import MyPrelude
import Utils.TH (rTrim)

-- | Ranges are [inclusive, exclusive), but note that Positions are interpreted
-- as being between characters thus the exclusiveness of the end doesn't really
-- apply most of the time
positionInRange :: Position -> Range -> Bool
positionInRange
  (Position l c)
  (Range (Position lStart cStart) (Position lEnd cEnd)) =
    (l > lStart || l == lStart && c >= cStart)
      && (l < lEnd || l == lEnd && c < cEnd)

onLine :: Int -> Range
onLine (fromIntegral -> line) = Range (Position line 0) (Position (line + 1) 0)

-- | For cases where we don't have a start and an end point start the range at
-- the current location, and go until the start of the next line, as this is
-- the best estimate of the precise range of the error that we can give.
atLineCol :: Int -> Int -> Range
atLineCol line col = rangeFromPosition (posAtLineCol line col)

posAtLineCol :: Int -> Int -> Position
posAtLineCol (fromIntegral -> line) (fromIntegral -> col) =
  Position line col

-- | For cases where we don't have a start and an end point start the range at
-- the current location, and go until the start of the next line, as this is
-- the best estimate of the precise range of the error that we can give.
rangeFromPosition :: Position -> Range
rangeFromPosition p =
  Range p (colOnNextLine p 0)

colOnNextLine :: Position -> Int -> Position
colOnNextLine (Position line _) (fromIntegral -> col) = Position (line + 1) col

sameLineWithCol :: Position -> Int -> Position
sameLineWithCol (Position line _) (fromIntegral -> col) = Position line col

nextCol :: Position -> Position
nextCol (Position line col) = Position line (col + 1)

rangeFromStartOfLine :: Position -> Range
rangeFromStartOfLine p = Range (sameLineWithCol p 0) p

rangeForLine :: Position -> Range
rangeForLine (Position l _) = onLine (fromIntegral l)

restrictToRange :: Text -> Range -> Text
restrictToRange
  text
  ( Range
      (Position (fromIntegral -> ls) (fromIntegral -> cs))
      (Position (fromIntegral -> le) (fromIntegral -> ce))
    ) =
    let relevantLines = take (le - ls + 1) $ drop ls $ lines text
        amountToTakeFromLastLine
          | ls == le = ce - cs
          | le - ls + 1 > length relevantLines = maxBound
          | otherwise = ce
        endsWithNewline = "\n" `isSuffixOf` text
     in relevantLines
          & (_head %~ drop cs)
          & (_last %~ if endsWithNewline then (++ "\n") else id)
          & (_last %~ take amountToTakeFromLastLine)
          & intercalate "\n"

pattern P :: Int -> Int -> Position
pattern P l c <- Position (fromIntegral -> l) (fromIntegral -> c)
  where
    P l c = Position (fromIntegral l) (fromIntegral c)

pattern R :: Position -> Position -> Range
pattern R s e = Range s e

spec_restrictToRange :: Spec
spec_restrictToRange = do
  let multilineSample =
        [rTrim|
          Some sample text.
          It is multiple lines.
          This is the third line.
        |]
  let restrictsMultilineSampleTo r expected =
        it (show r <> " restricts multiline sample to")
          $ (multilineSample `restrictToRange` r)
          `shouldBe` expected
  R (P 0 0) (P 0 4) `restrictsMultilineSampleTo` "Some"
  R (P 0 5) (P 0 11) `restrictsMultilineSampleTo` "sample"
  R (P 0 0) (P 1 0) `restrictsMultilineSampleTo` "Some sample text.\n"
  R (P 1 0) (P 2 0) `restrictsMultilineSampleTo` "It is multiple lines.\n"
  R (P 1 0) (P 2 4) `restrictsMultilineSampleTo` "It is multiple lines.\nThis"
  R (P 0 5) (P 1 2) `restrictsMultilineSampleTo` "sample text.\nIt"
  R (P 0 5) (P 2 4) `restrictsMultilineSampleTo` "sample text.\nIt is multiple lines.\nThis"
  R (P 0 0) (P 3 0) `restrictsMultilineSampleTo` multilineSample
  -- any end beyond the end of the text should include everything
  R (P 0 0) (P 10 0) `restrictsMultilineSampleTo` multilineSample
