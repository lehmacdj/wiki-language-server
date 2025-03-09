module Handlers.Prelude
  ( module Handlers.Prelude,
    module X,
  )
where

import Language.LSP.Protocol.Lens as J hiding (to)
import Language.LSP.Protocol.Message as X
import Language.LSP.Protocol.Types as X
import Language.LSP.Server as X
import Language.LSP.VFS as X
import Models.Page.Parser qualified as Page
import Models.WikiLanguageServerConfig
import MyPrelude
import Text.Pandoc.Definition (Pandoc)
import Utils.LSP
import Utils.Logging

type MonadTResponseError method m = MonadError (TResponseError method) m

type Response (m :: Method 'ClientToServer 'Request) =
  Either (TResponseError m) (MessageResult m)

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
