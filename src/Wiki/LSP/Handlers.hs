-- 99.9% of functions in this module have type Handlers m, and it is redundant
-- to specify that every time.
{-# OPTIONS_GHC -Wno-missing-signatures #-}

-- | Handlers for the LSP client methods.
module Wiki.LSP.Handlers (handlers) where

import Language.LSP.Server
import Language.LSP.Types
import Language.LSP.Types.Lens as J
import Language.LSP.VFS
import MyPrelude
import Text.Pandoc.Definition (Pandoc)
import Wiki.Diagnostics
import Wiki.LSP.Config
import Wiki.LSP.Util
import Wiki.LinkTarget
import Wiki.Page qualified as Page

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

type Response (m :: Method 'FromClient 'Request) =
  Either ResponseError (ResponseResult m)

textDocumentDefinition ::
  HandlerMonad m =>
  RequestMessage 'TextDocumentDefinition ->
  m (Response 'TextDocumentDefinition)
textDocumentDefinition request = runExceptT $ do
  (nuri, _version, mcontents) <- lift $ tryGetContents request
  contents <- onNothing mcontents $ do
    throwError $
      ResponseError
        { _code = UnknownErrorCode, -- TODO: replace with ResponseFailure when available
          _message = "Failed to retrieve text from requested document.",
          _xdata = Nothing
        }
  parsed <- onLeft (parseDocument nuri contents) \_diagnostic ->
    -- we will update the diagnostics on the didChange request if we didn't
    -- already
    throwError $
      ResponseError
        { _code = InternalError,
          _message = "Current document state doesn't parse",
          _xdata = Nothing
        }
  let position = request ^. J.params . J.position
  case Page.getLinkTargetAtPosition parsed position of
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

-- TODO: Why does requestHandler allow more complicated response patterns than
-- this?. Is there an exception that can be caught to determine if the response
-- was received and retry? Is there a reason why it would be desireable to send
-- a response several times? possibly it's just to avoid Monad constraint on m?

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
      requestHandler' STextDocumentDefinition textDocumentDefinition
    ]
