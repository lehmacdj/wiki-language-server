module Handlers.TextDocument.Definition where

import Handlers.Prelude
import Language.LSP.Protocol.Lens as J hiding (to)
import Models.LinkTarget
import Models.Page.GotoDefinition qualified as GotoDefinition
import Models.Page.Utils qualified as Page
import MyPrelude
import Utils.RangePosition

textDocumentDefinition ::
  (Logging :> es, VFSAccess :> es, FileSystem :> es) =>
  HandlerFor 'Method_TextDocumentDefinition es
textDocumentDefinition request = withEarlyReturn $ do
  let nuri = uriFromMessage request
  mVersionContents <- tryGetVfsUriContents nuri
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
  targetNuri <- relativeToWorkingDirectory link
  let targetUri = fromNormalizedUri targetNuri
  let targetLocation p = InL . Definition . InL $ Location targetUri p
  (_targetNuriVersion, mTargetContents) <- tryGetUriContents targetNuri
  targetContents <-
    onNothing mTargetContents . returnEarly $ targetLocation (atLineCol 0 0)
  targetParsed <-
    onLeft
      (parseDocument targetNuri targetContents)
      (const . returnEarly $ targetLocation (atLineCol 0 0))
  pos <-
    onNothing
      (Page.getFirstH1Position targetParsed)
      (returnEarly . targetLocation $ atLineCol 0 0)
  pure $ targetLocation (rangeFromPosition pos)
