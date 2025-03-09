module Handlers.TextDocument.Formatting where

import Handlers.Prelude
import Models.Page.Formatting qualified as Formatting
import Models.Page.Utils qualified as Page
import Models.Slug qualified as Slug
import Models.WikiLanguageServerConfig
import MyPrelude
import System.Directory (getCurrentDirectory)
import Text.Pandoc.Definition (Pandoc)

pageForSlug ::
  (MonadLsp Config m, MonadTResponseError method m) => Text -> m (Maybe Pandoc)
pageForSlug slug = withEarlyReturn do
  currentDirectory <- liftIO getCurrentDirectory
  let uri = toNormalizedUri $ Slug.intoUri currentDirectory slug
  (_, mContents) <- tryGetUriContents uri
  contents <- onNothing mContents . returnEarly $ Nothing @Pandoc
  pure . justFromRight $ parseDocument uri contents

-- | Get the title for a slug returning Nothing if the file isn't found or we
-- can't find the title in the page
titleForSlug ::
  (MonadLsp Config m, MonadTResponseError method m) => Text -> m (Maybe Text)
titleForSlug slug = withEarlyReturn do
  mPage <- pageForSlug slug
  page <- onNothing mPage . returnEarly $ Nothing @Text
  pure $ Page.getTitle page

textDocumentFormatting ::
  (MonadLsp Config m) =>
  TRequestMessage 'Method_TextDocumentFormatting ->
  m (Response 'Method_TextDocumentFormatting)
textDocumentFormatting request = runExceptionErrorT $ do
  let uri = uriFromMessage request
  mVersionContents <- tryGetVfsUriContents uri
  (_, contents) <- onNothing mVersionContents throwNoContentsAvailable
  parsed <- parseDocumentThrow uri contents
  let edits = Formatting.editsForPage parsed
  resolvedEdits <- traverse (Formatting.textEditOfOperation titleForSlug) edits
  pure . InL $ concat resolvedEdits
