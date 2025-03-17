module Handlers.TextDocument.Formatting where

import Effectful.FileSystem (getCurrentDirectory)
import Handlers.Prelude
import Models.Page.Formatting qualified as Formatting
import Models.Page.Utils qualified as Page
import Models.Slug qualified as Slug
import MyPrelude
import Text.Pandoc.Definition (Pandoc)

pageForSlug ::
  (VFSAccess :> es, Logging :> es, FileSystem :> es) =>
  Text -> Eff es (Maybe Pandoc)
pageForSlug slug = withEarlyReturn do
  currentDirectory <- getCurrentDirectory
  let uri = toNormalizedUri $ Slug.intoUri currentDirectory slug
  (_, mContents) <- tryGetUriContents uri
  contents <- onNothing mContents . returnEarly $ Nothing @Pandoc
  pure . justFromRight $ parseDocument uri contents

-- | Get the title for a slug returning Nothing if the file isn't found or we
-- can't find the title in the page
titleForSlug ::
  (Logging :> es, VFSAccess :> es, FileSystem :> es) =>
  Text -> Eff es (Maybe Text)
titleForSlug slug = withEarlyReturn do
  mPage <- pageForSlug slug
  page <- onNothing mPage . returnEarly $ Nothing @Text
  pure $ Page.getTitle page

textDocumentFormatting ::
  (Logging :> es, VFSAccess :> es, FileSystem :> es) =>
  HandlerFor 'Method_TextDocumentFormatting es
textDocumentFormatting request = do
  let uri = uriFromMessage request
  mVersionContents <- tryGetVfsUriContents uri
  (_, contents) <- onNothing mVersionContents throwNoContentsAvailable
  parsed <- parseDocumentThrow uri contents
  let edits = Formatting.editsForPage parsed
  resolvedEdits <- traverse (Formatting.textEditOfOperation titleForSlug) edits
  pure . InL $ concat resolvedEdits
