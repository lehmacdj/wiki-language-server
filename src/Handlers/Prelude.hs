module Handlers.Prelude
  ( module Handlers.Prelude,
    module X,
  )
where

import Control.Monad.Except (MonadError)
import LSP.Raw as X
import LSP.VFS as X
import Language.LSP.Protocol.Lens as J hiding (to)
import Language.LSP.Protocol.Message as X
import Language.LSP.Protocol.Types as X
import Language.LSP.Server as X (Handlers, MonadLsp, notificationHandler, requestHandler)
import Language.LSP.VFS
import Models.Page.Parser qualified as Page
import MyPrelude
import Text.Pandoc.Definition (Pandoc)
import Utils.LSP
import Utils.Logging as X

type MonadTResponseError method m = MonadError (TResponseError method) m

type HandlerFor (m :: Method 'ClientToServer 'Request) es =
  TRequestMessage m -> Eff (Error (TResponseError m) : es) (MessageResult m)

uriFromMessage ::
  ( HasParams msg p,
    HasTextDocument p t,
    HasUri t Uri
  ) =>
  msg -> NormalizedUri
uriFromMessage = view $ J.params . J.textDocument . J.uri . to toNormalizedUri

tryGetVfsUriContents ::
  (VFSAccess :> es) =>
  NormalizedUri ->
  Eff es (Maybe (Int32, Text))
tryGetVfsUriContents nuri =
  getVirtualFile nuri <&> \case
    Nothing -> Nothing
    Just vf -> Just (virtualFileVersion vf, virtualFileText vf)

tryGetUriContents ::
  (Logging :> es, FileSystem :> es, VFSAccess :> es) =>
  NormalizedUri ->
  -- | if the version is provided, the contents were found in the VFS
  -- otherwise we read the contents from the filesystem if it isn't Nothing
  Eff es (Maybe Int32, Maybe Text)
tryGetUriContents nuri = withEarlyReturn do
  mVfsContents <- tryGetVfsUriContents nuri
  case mVfsContents of
    Just (version, contents) -> returnEarly (Just version, Just contents)
    Nothing -> pure ()
  filePath <-
    onNothing
      (nuriToFilePath nuri)
      (returnEarly (Nothing @Int32, Nothing @Text))
  try @_ @SomeException (readFileUtf8 filePath) <&> \case
    Left _ -> (Nothing, Nothing)
    Right contents -> (Nothing, Just contents)

parseDocument :: NormalizedUri -> Text -> Either Diagnostic Pandoc
parseDocument nuri = Page.parse (fromMaybe "<unknown>" $ nuriToFilePath nuri)

parseDocumentThrow ::
  (Logging :> es, Error (TResponseError method) :> es) =>
  NormalizedUri -> Text -> Eff es Pandoc
parseDocumentThrow nuri contents =
  onLeft (parseDocument nuri contents) (const throwDocumentStateDoesNotParse)

throwNoContentsAvailable ::
  (Logging :> es, Error (TResponseError method) :> es) => Eff es a
throwNoContentsAvailable = do
  let msg = "Failed to retrieve text from requested document."
  logError msg
  throwError_
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
  (Logging :> es, Error (TResponseError method) :> es) => Eff es a
throwDocumentStateDoesNotParse = do
  let msg = "Current document state doesn't parse"
  logError msg
  throwError_
    $ TResponseError
      { _code = InL LSPErrorCodes_RequestFailed,
        _message = msg,
        _xdata = Nothing
      }

rootExceptionHandler ::
  (Logging :> es, IOE :> es, Error (TResponseError method) :> es) =>
  Eff es a ->
  Eff es a
rootExceptionHandler action =
  action `catchAny` \e -> do
    let msg = "Encountered unrecoverable IO error during request: " <> tshow e
    logError msg
    throwError_
      $ TResponseError
        { _code = InR ErrorCodes_InternalError,
          _message = msg,
          _xdata = Nothing
        }
