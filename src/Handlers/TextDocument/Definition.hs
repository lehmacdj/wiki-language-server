module Handlers.TextDocument.Definition where

import Handlers.Prelude
import Language.LSP.Protocol.Lens as J hiding (to)
import Models.LinkTarget
import Models.Page.GotoDefinition qualified as GotoDefinition
import Models.Page.Utils qualified as Page
import Models.WikiLanguageServerConfig
import MyPrelude
import Utils.LSP

textDocumentDefinition ::
  (MonadLsp Config m) =>
  TRequestMessage 'Method_TextDocumentDefinition ->
  m (Response 'Method_TextDocumentDefinition)
textDocumentDefinition request = runExceptionErrorT . withEarlyReturn $ do
  let nuri = uriFromMessage request
  mVersionContents <- lift $ tryGetVfsUriContents nuri
  (_, contents) <- onNothing mVersionContents throwNoContentsAvailable
  parsed <- parseDocumentThrow nuri contents
  let position = request ^. J.params . J.position
  link <-
    onNothing
      (GotoDefinition.getLinkTargetAtPosition parsed position)
      ( returnEarly @(MessageResult 'Method_TextDocumentDefinition)
          (InR . InL $ [])
      )
  -- We have the target link, now we just need to try to figure out where to
  -- jump within it. We try to compute the location of the first non-heading
  -- paragraph defaulting to the start of the document if we can't find one
  targetUri <- relativeToWorkingDirectory link
  let targetLocation p = InL . Definition . InL $ Location targetUri p
  let targetNuri = toNormalizedUri targetUri
  (_targetNuriVersion, mTargetContents) <- tryGetUriContents targetNuri
  targetContents <-
    onNothing mTargetContents . returnEarly $ targetLocation (atLineCol 0 0)
  targetParsed <-
    onLeft
      (parseDocument targetNuri targetContents)
      (const . returnEarly $ targetLocation (atLineCol 0 0))
  pos <-
    onNothing
      (Page.getFirstLineAfterFirstH1 targetParsed)
      (returnEarly . targetLocation $ atLineCol 0 0)
  pure $ targetLocation (rangeFromPosition pos)
