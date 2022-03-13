module Wiki.LSP.Util where

import Language.LSP.Diagnostics
import Language.LSP.Server
import Language.LSP.Types
import MyPrelude
import Wiki.LSP.Config

sendDiagnostics ::
  MonadLsp Config m => NormalizedUri -> TextDocumentVersion -> [Diagnostic] -> m ()
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
