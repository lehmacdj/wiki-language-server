-- | Handlers for the LSP client methods.
module Wiki.LSP.Handlers (handlers) where

import Language.LSP.Protocol.Lens as J hiding (to)
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Language.LSP.Server
import Language.LSP.VFS
import MyPrelude
import System.Directory (getCurrentDirectory)
import Text.Pandoc.Definition (Pandoc)
import Wiki.Diagnostics
import Wiki.LSP.Config
import Wiki.LSP.Logging
import Wiki.LSP.Util
import Wiki.LinkTarget
import Wiki.Page.Formatting qualified as Formatting
import Wiki.Page.GotoDefinition qualified as GotoDefinition
import Wiki.Page.Parser qualified as Page
import Wiki.Page.Utils qualified as Page
import Wiki.Slug qualified as Slug

type MonadTResponseError method m = MonadError (TResponseError method) m

initialized ::
  (MonadLsp Config m) => TNotificationMessage 'Method_Initialized -> m ()
initialized _n = pure ()

textDocumentDidOpen ::
  (MonadLsp Config m) => TNotificationMessage 'Method_TextDocumentDidOpen -> m ()
textDocumentDidOpen notification = do
  let doc = notification ^. J.params . J.textDocument
      nuri = doc ^. J.uri . to toNormalizedUri
      version = doc ^. J.version . to Just
      contents = doc ^. J.text
  case Page.parse (fromMaybe "<unknown>" $ nuriToFilePath nuri) contents of
    Left d ->
      sendDiagnostics nuri version [d]
    Right _ -> pure ()

textDocumentDidChange ::
  (MonadLsp Config m) => TNotificationMessage 'Method_TextDocumentDidChange -> m ()
textDocumentDidChange notification = withEarlyReturn $ do
  (nuri, mVersionContents) <- tryGetContents notification
  (version, contents) <- onNothing mVersionContents $ returnEarly ()
  case parseDocument nuri contents of
    Left d ->
      sendDiagnostics nuri (Just version) [d]
    Right _ ->
      sendDiagnostics
        nuri
        (Just version)
        [mkDiagnostic GeneralInfo (atLineCol 0 0) "didChange: Parsed successfully!"]

tryGetContents ::
  ( MonadLsp Config m,
    HasParams msg p,
    HasTextDocument p t,
    HasUri t Uri
  ) =>
  msg ->
  m (NormalizedUri, Maybe (Int32, Text))
tryGetContents message =
  let uri = message ^. J.params . J.textDocument . J.uri
   in tryGetVfsUriContents uri

tryGetVfsUriContents ::
  (MonadLsp Config m) =>
  Uri ->
  m (NormalizedUri, Maybe (Int32, Text))
tryGetVfsUriContents uri = do
  let nuri = toNormalizedUri uri
  getVirtualFile nuri <&> \case
    Nothing -> (nuri, Nothing)
    Just vf -> (nuri, Just (virtualFileVersion vf, virtualFileText vf))

tryGetUriContents ::
  (MonadLsp Config m) =>
  Uri ->
  -- | if the version is provided, the contents were found in the VFS
  -- otherwise we read the contents from the filesystem if it isn't Nothing
  m (NormalizedUri, Maybe Int32, Maybe Text)
tryGetUriContents uri = withEarlyReturn do
  (nuri, mVfsContents) <- tryGetVfsUriContents uri
  case mVfsContents of
    Just (version, contents) -> returnEarly (nuri, Just version, Just contents)
    Nothing -> pure ()
  filePath <-
    onNothing
      (nuriToFilePath nuri)
      (returnEarly (nuri, Nothing @Int32, Nothing @Text))
  try @_ @IOError (readFileUtf8 filePath) <&> \case
    Left _ -> (nuri, Nothing, Nothing)
    Right contents -> (nuri, Nothing, Just contents)

parseDocument :: NormalizedUri -> Text -> Either Diagnostic Pandoc
parseDocument nuri =
  Page.parse (fromMaybe "<unknown>" $ nuriToFilePath nuri)

parseDocumentThrow ::
  (MonadTResponseError method m, MonadLsp c m) =>
  NormalizedUri -> Text -> m Pandoc
parseDocumentThrow nuri contents =
  onLeft (parseDocument nuri contents) (const throwDocumentStateDoesNotParse)

type Response (m :: Method 'ClientToServer 'Request) =
  Either (TResponseError m) (MessageResult m)

throwNoContentsAvailable ::
  (MonadLsp c m, MonadTResponseError method m) => m a
throwNoContentsAvailable = do
  let msg = "Failed to retrieve text from requested document."
  logError msg
  throwError
    $ TResponseError
      { _code = InL LSPErrorCodes_RequestFailed,
        _message = msg,
        _xdata = Nothing
      }

-- | It is generally ok to use this if you need a parsed document in a request.
--
-- We will update the diagnostics on the didChange request if we didn't already
-- so it isn't necessary to report the parse failure at use sites of this
-- function.
throwDocumentStateDoesNotParse ::
  (MonadLsp c m, MonadError (TResponseError method) m) => m a
throwDocumentStateDoesNotParse = do
  let msg = "Current document state doesn't parse"
  logError msg
  throwError
    $ TResponseError
      { _code = InL LSPErrorCodes_RequestFailed,
        _message = msg,
        _xdata = Nothing
      }

_rethrowIOException ::
  (MonadUnliftIO m, MonadError (TResponseError method) m, MonadLsp c m) =>
  m a ->
  m a
_rethrowIOException action =
  action `catch` \(ioe :: IOError) -> do
    let msg = "Encountered unrecoverable IO error during request: " <> tshow ioe
    logError msg
    throwError
      $ TResponseError
        { _code = InR ErrorCodes_InternalError,
          _message = msg,
          _xdata = Nothing
        }

textDocumentDefinition ::
  (MonadLsp Config m) =>
  TRequestMessage 'Method_TextDocumentDefinition ->
  m (Response 'Method_TextDocumentDefinition)
textDocumentDefinition request = runExceptionErrorT . withEarlyReturn $ do
  (nuri, mVersionContents) <- lift $ tryGetContents request
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
  (targetNuri, _targetNuriVersion, mTargetContents) <- tryGetUriContents targetUri
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

pageForSlug ::
  (MonadLsp Config m, MonadTResponseError method m) => Text -> m (Maybe Pandoc)
pageForSlug slug = withEarlyReturn do
  currentDirectory <- liftIO getCurrentDirectory
  let uri = Slug.intoUri currentDirectory slug
  (nuri, _, mContents) <- tryGetUriContents uri
  contents <- onNothing mContents . returnEarly $ Nothing @Pandoc
  pure . justFromRight $ parseDocument nuri contents

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
  (nuri, mVersionContents) <- lift $ tryGetContents request
  (_, contents) <- onNothing mVersionContents throwNoContentsAvailable
  parsed <- parseDocumentThrow nuri contents
  let edits = Formatting.editsForPage parsed
  resolvedEdits <- traverse (Formatting.textEditOfOperation titleForSlug) edits
  pure . InL $ concat resolvedEdits

-- | Shim for making requestHandler easier to use in the simple way that one
-- generally wants to use it
requestHandler' ::
  forall (m :: Method 'ClientToServer 'Request) f.
  (Monad f) =>
  SMethod m ->
  (TRequestMessage m -> f (Either (TResponseError m) (MessageResult m))) ->
  Handlers f
requestHandler' s handler = requestHandler s \request responder -> do
  handler request >>= responder

handlers :: (MonadLsp Config m) => Handlers m
handlers =
  mconcat
    [ notificationHandler SMethod_Initialized initialized,
      notificationHandler SMethod_TextDocumentDidOpen textDocumentDidOpen,
      notificationHandler SMethod_TextDocumentDidChange textDocumentDidChange,
      requestHandler' SMethod_TextDocumentDefinition textDocumentDefinition,
      requestHandler' SMethod_TextDocumentFormatting textDocumentFormatting
    ]
