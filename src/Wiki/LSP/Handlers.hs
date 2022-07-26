-- | Handlers for the LSP client methods.
module Wiki.LSP.Handlers (handlers) where

import Data.Text.IO qualified as Text
import Language.LSP.Server
import Language.LSP.Types
import Language.LSP.Types.Lens as J
import Language.LSP.VFS
import MyPrelude
import System.Directory (getCurrentDirectory)
import Text.Pandoc.Definition (Pandoc)
import Wiki.Diagnostics
import Wiki.LSP.Config
import Wiki.LSP.Util
import Wiki.LinkTarget
import Wiki.Page.Formatting qualified as Formatting
import Wiki.Page.GotoDefinition qualified as GotoDefinition
import Wiki.Page.Parser qualified as Page
import Wiki.Page.Utils qualified as Page
import Wiki.Slug qualified as Slug

type HandlerMonad m = (MonadLsp Config m)

initialized ::
  HandlerMonad m => NotificationMessage 'Initialized -> m ()
initialized _n = pure ()

textDocumentDidOpen ::
  HandlerMonad m => NotificationMessage 'TextDocumentDidOpen -> m ()
textDocumentDidOpen notification = do
  let doc = notification ^. J.params . J.textDocument
      nuri = doc ^. J.uri . to toNormalizedUri
      version = doc ^. J.version . to Just
      contents = doc ^. J.text
  case Page.parse (fromMaybe "<unknown>" $ nuriToFilePath nuri) contents of
    Left d ->
      sendDiagnostics nuri version [d]
    Right _ ->
      sendDiagnostics
        nuri
        version
        [mkDiagnostic GeneralInfo (atLineCol 0 0) "didOpen: Parsed successfully!"]

textDocumentDidChange ::
  HandlerMonad m => NotificationMessage 'TextDocumentDidChange -> m ()
textDocumentDidChange notification = runEarlyReturnT $ do
  (nuri, version, mcontents) <- tryGetContents notification
  contents <- onNothing mcontents $ returnEarly ()
  case parseDocument nuri contents of
    Left d ->
      sendDiagnostics nuri version [d]
    Right _ ->
      sendDiagnostics
        nuri
        version
        [mkDiagnostic GeneralInfo (atLineCol 0 0) "didChange: Parsed successfully!"]

tryGetContents ::
  ( MonadLsp Config m,
    HasParams msg p,
    HasTextDocument p t,
    HasUri t Uri
  ) =>
  msg ->
  m (NormalizedUri, TextDocumentVersion, Maybe Text)
tryGetContents message = do
  let nuri =
        message
          ^. J.params
            . J.textDocument
            . J.uri
            . to toNormalizedUri
  getVirtualFile nuri <&> \case
    Nothing -> (nuri, Nothing, Nothing)
    Just vf -> (nuri, Just (virtualFileVersion vf), Just (virtualFileText vf))

parseDocument :: NormalizedUri -> Text -> Either Diagnostic Pandoc
parseDocument nuri =
  Page.parse (fromMaybe "<unknown>" $ nuriToFilePath nuri)

parseDocumentThrow ::
  MonadError ResponseError m => NormalizedUri -> Text -> m Pandoc
parseDocumentThrow nuri contents =
  onLeft (parseDocument nuri contents) (const throwDocumentStateDoesNotParse)

type Response (m :: Method 'FromClient 'Request) =
  Either ResponseError (ResponseResult m)

throwNoContentsAvailable :: MonadError ResponseError m => m a
throwNoContentsAvailable =
  throwError $
    ResponseError
      { _code = UnknownErrorCode, -- TODO: replace with ResponseFailure when available
        _message = "Failed to retrieve text from requested document.",
        _xdata = Nothing
      }

-- | It is generally ok to use this if you need a parsed document in a request.
--
-- We will update the diagnostics on the didChange request if we didn't already
-- so it isn't necessary to report the parse failure at use sites of this
-- function.
throwDocumentStateDoesNotParse :: MonadError ResponseError m => m a
throwDocumentStateDoesNotParse =
  throwError $
    ResponseError
      { _code = InternalError,
        _message = "Current document state doesn't parse",
        _xdata = Nothing
      }

textDocumentDefinition ::
  HandlerMonad m =>
  RequestMessage 'TextDocumentDefinition ->
  m (Response 'TextDocumentDefinition)
textDocumentDefinition request = runExceptT $ do
  (nuri, _version, mcontents) <- lift $ tryGetContents request
  contents <- onNothing mcontents throwNoContentsAvailable
  parsed <- parseDocumentThrow nuri contents
  let position = request ^. J.params . J.position
  case GotoDefinition.getLinkTargetAtPosition parsed position of
    Nothing ->
      pure . InR . InL $ List []
    Just link -> do
      uri <- relativeToWorkingDirectory link
      -- TODO: improve this by jumping to the start of text and skipping yaml
      -- front matter + links etc. that might be at the start of the document
      -- might even be better to jump to the first line of the body and skip
      -- the title
      let targetLocation = Location uri (atLineCol 0 0)
      pure $ InL targetLocation

-- | Get the title for a slug returning Nothing if the file isn't found or we
-- can't find the title in the page
titleForSlug ::
  (HandlerMonad m, MonadError ResponseError m) => Text -> m (Maybe Text)
titleForSlug slug = do
  currentDirectory <- liftIO getCurrentDirectory
  let path = Slug.intoNormalizedUri currentDirectory slug
  m_vf <- getVirtualFile path
  m_contents <- case m_vf of
    Nothing ->
      tryIOException . liftIO . Text.readFile $
        Slug.intoFilePathRelativeToDir currentDirectory slug
    Just vf -> pure . Just $ virtualFileText vf
  m_page <- for m_contents $ parseDocumentThrow path
  pure $ Page.getTitle =<< m_page

textDocumentFormatting ::
  HandlerMonad m =>
  RequestMessage 'TextDocumentFormatting ->
  m (Response 'TextDocumentFormatting)
textDocumentFormatting request = runExceptionErrorT $ do
  (nuri, _version, mcontents) <- lift $ tryGetContents request
  contents <- onNothing mcontents throwNoContentsAvailable
  parsed <- parseDocumentThrow nuri contents
  let edits = Formatting.editsForPage parsed
  List . concat <$> traverse (Formatting.textEditOfOperation titleForSlug) edits

-- | Shim for making requestHandler easier to use in the simple way that one
-- generally wants to use it
requestHandler' ::
  forall (m :: Method 'FromClient 'Request) f.
  Monad f =>
  SMethod m ->
  (RequestMessage m -> f (Either ResponseError (ResponseResult m))) ->
  Handlers f
requestHandler' s handler = requestHandler s \request responder ->
  handler request >>= responder

handlers :: HandlerMonad m => Handlers m
handlers =
  mconcat
    [ notificationHandler SInitialized initialized,
      notificationHandler STextDocumentDidOpen textDocumentDidOpen,
      notificationHandler STextDocumentDidChange textDocumentDidChange,
      requestHandler' STextDocumentDefinition textDocumentDefinition,
      requestHandler' STextDocumentFormatting textDocumentFormatting
    ]
