module Utils.LSP where

import Language.LSP.Diagnostics
import Language.LSP.Protocol.Types
import Language.LSP.Server
import MyPrelude
import Models.WikiLanguageServerConfig

sendDiagnostics ::
  (MonadLsp Config m) => NormalizedUri -> Maybe Int32 -> [Diagnostic] -> m ()
sendDiagnostics uri version diagnostics = do
  Config {maxDiagnostics} <- getConfig
  publishDiagnostics maxDiagnostics uri version (partitionBySource diagnostics)

nuriToFilePath :: NormalizedUri -> Maybe FilePath
nuriToFilePath = uriToFilePath . fromNormalizedUri

-- | Ranges are [inclusive, exclusive)
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
