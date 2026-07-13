module LSP.Mutation.Integration (spec_lspMutation) where

import Control.Applicative.Combinators (skipManyTill)
import Control.Concurrent qualified as Concurrent
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS.Char8
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Language.LSP.Test qualified as LspTest
import Models.Slug qualified as Slug
import MyPrelude
import System.Directory qualified as Directory
import System.FilePath (takeExtension)
import System.Posix.IO qualified as Posix
import System.Process
  ( CreateProcess (cwd, std_err, std_in, std_out),
    ProcessHandle,
    StdStream (CreatePipe, Inherit),
    createProcess,
    proc,
    terminateProcess,
    waitForProcess,
  )

data WikiFixture = WikiFixture
  { root :: FilePath,
    state :: FilePath
  }

spec_lspMutation :: Spec
spec_lspMutation = describe "LSP wiki mutations" do
  it "returns from creation and immediately resolves the new note" $
    withWikiFixture \fixture -> runWikiSession fixture do
      document <- LspTest.openDoc "source.md" "markdown"
      requestId <- sendCreateRequest document
      updated <-
        skipManyTill
          LspTest.anyNotification
          (LspTest.getDocumentEdit document)
      void
        ( LspTest.responseForId
            SMethod_WorkspaceExecuteCommand
            requestId ::
            LspTest.Session
              (TResponseMessage 'Method_WorkspaceExecuteCommand)
        )
      liftIO $
        updated
          `shouldSatisfy` Text.isInfixOf "|Parent concept]]<!--wls-->"
      definitions <-
        LspTest.getDefinitions document $ Position 2 4
      liftIO $ definitions `shouldSatisfy` hasDefinition

  it "rolls a staged note back when the client rejects the edit" $
    withWikiFixture \fixture -> runWikiRejectingSession fixture \proxy -> do
      document <- LspTest.openDoc "source.md" "markdown"
      requestId <- sendCreateRequest document
      liftIO $ Concurrent.takeMVar proxy.applyRequestSeen
      stagedFiles <- liftIO $ listMarkdownFiles fixture.root
      liftIO $ stagedFiles `shouldSatisfy` ((== 2) . length)
      liftIO $ Concurrent.putMVar proxy.allowRejection ()
      void
        ( LspTest.responseForId
            SMethod_WorkspaceExecuteCommand
            requestId ::
            LspTest.Session
              (TResponseMessage 'Method_WorkspaceExecuteCommand)
        )
      rolledBack <- liftIO $ eventually do
        markdownFiles <- listMarkdownFiles fixture.root
        pure $ markdownFiles == ["source.md"]
      liftIO $ rolledBack `shouldBe` True

sendCreateRequest ::
  TextDocumentIdentifier ->
  LspTest.Session (LspId 'Method_WorkspaceExecuteCommand)
sendCreateRequest document =
  LspTest.sendRequest
    SMethod_WorkspaceExecuteCommand
    ExecuteCommandParams
      { _workDoneToken = Nothing,
        _command = "wiki.createNoteFromSelection",
        _arguments =
          Just
            [ Aeson.object
                [ "textDocument" Aeson..= document,
                  "range" Aeson..= Range (Position 2 2) (Position 2 16),
                  "openAfterCreation" Aeson..= False
                ]
            ]
      }

hasDefinition :: Definition |? ([DefinitionLink] |? Null) -> Bool
hasDefinition = \case
  InL (Definition (InL _)) -> True
  InL (Definition (InR locations)) -> not $ null locations
  InR (InL links) -> not $ null links
  InR (InR _) -> False

runWikiSession :: WikiFixture -> LspTest.Session a -> IO a
runWikiSession fixture =
  LspTest.runSessionWithConfigCustomProcess
    (\process -> process {cwd = Just fixture.root})
    LspTest.defaultConfig
    command
    LspTest.fullLatestClientCaps
    fixture.root
  where
    command =
      "env XDG_STATE_HOME="
        <> fixture.state
        <> " wiki language-server"

data RejectingProxy = RejectingProxy
  { clientIn :: Handle,
    clientOut :: Handle,
    clientToProxyRead :: Handle,
    proxyToClientWrite :: Handle,
    serverIn :: Handle,
    serverOut :: Handle,
    serverProcess :: ProcessHandle,
    forwardingThreads :: [Concurrent.ThreadId],
    applyRequestSeen :: Concurrent.MVar (),
    allowRejection :: Concurrent.MVar ()
  }

runWikiRejectingSession ::
  WikiFixture ->
  (RejectingProxy -> LspTest.Session a) ->
  IO a
runWikiRejectingSession fixture session =
  -- lsp-test eagerly accepts and applies workspace edits. Intercept only that
  -- request so the rejection and server-side rollback path can be exercised.
  bracket (startRejectingProxy fixture) stopRejectingProxy \proxy ->
    LspTest.runSessionWithHandles
      proxy.clientIn
      proxy.clientOut
      LspTest.defaultConfig
      LspTest.fullLatestClientCaps
      fixture.root
      (session proxy)

startRejectingProxy :: WikiFixture -> IO RejectingProxy
startRejectingProxy fixture = do
  (Just serverIn, Just serverOut, _, serverProcess) <-
    createProcess
      (proc "env" ["XDG_STATE_HOME=" <> fixture.state, "wiki", "language-server"])
        { cwd = Just fixture.root,
          std_in = CreatePipe,
          std_out = CreatePipe,
          -- The server logger writes to stderr; closing it aborts startup.
          std_err = Inherit
        }
  (clientToProxyReadFd, clientInFd) <- Posix.createPipe
  (clientOutFd, proxyToClientWriteFd) <- Posix.createPipe
  clientToProxyRead <- Posix.fdToHandle clientToProxyReadFd
  clientIn <- Posix.fdToHandle clientInFd
  clientOut <- Posix.fdToHandle clientOutFd
  proxyToClientWrite <- Posix.fdToHandle proxyToClientWriteFd
  for_
    [ serverIn,
      serverOut,
      clientToProxyRead,
      clientIn,
      clientOut,
      proxyToClientWrite
    ]
    (`hSetBuffering` NoBuffering)
  writeLock <- Concurrent.newMVar ()
  applyRequestSeen <- Concurrent.newEmptyMVar
  allowRejection <- Concurrent.newEmptyMVar
  clientThread <- Concurrent.forkIO do
    ignoreClosure $ forever do
      message <- readLspMessage clientToProxyRead
      Concurrent.withMVar writeLock \_ -> writeLspMessage serverIn message
    closeIgnoringErrors serverIn
  serverThread <- Concurrent.forkIO do
    ignoreClosure $ forever do
      message <- readLspMessage serverOut
      case applyEditRequestId message of
        Nothing -> writeLspMessage proxyToClientWrite message
        Just requestId -> do
          Concurrent.putMVar applyRequestSeen ()
          Concurrent.takeMVar allowRejection
          Concurrent.withMVar writeLock \_ ->
            writeLspMessage serverIn $ rejectionResponse requestId
    -- Signal EOF to lsp-test when the server finishes. Leaving this writer
    -- open makes session shutdown wait forever after the forwarding loop ends.
    closeIgnoringErrors proxyToClientWrite
  pure
    RejectingProxy
      { forwardingThreads = [clientThread, serverThread],
        ..
      }

stopRejectingProxy :: RejectingProxy -> IO ()
stopRejectingProxy proxy = do
  void $ Concurrent.tryPutMVar proxy.allowRejection ()
  traverse_ Concurrent.killThread proxy.forwardingThreads
  traverse_ closeIgnoringErrors
    [ proxy.clientIn,
      proxy.clientOut,
      proxy.clientToProxyRead,
      proxy.proxyToClientWrite,
      proxy.serverIn,
      proxy.serverOut
    ]
  void $ try @_ @SomeException $ terminateProcess proxy.serverProcess
  void $ try @_ @SomeException $ waitForProcess proxy.serverProcess

closeIgnoringErrors :: Handle -> IO ()
closeIgnoringErrors handle = void $ try @_ @SomeException $ hClose handle

ignoreClosure :: IO () -> IO ()
ignoreClosure action = void $ try @_ @SomeException action

readLspMessage :: Handle -> IO BS.ByteString
readLspMessage handle = do
  headers <- readHeaders
  contentLength <-
    headMay (mapMaybe parseContentLength headers)
      `onNothing` throwIO (userError "LSP message has no Content-Length")
  BS.hGet handle contentLength
  where
    readHeaders = go []
    go headers = do
      header <- BS.Char8.hGetLine handle
      if header == "\r"
        then pure $ reverse headers
        else go (header : headers)

    parseContentLength header = do
      value <- BS.Char8.stripPrefix "Content-Length: " header
      readMay . BS.Char8.unpack $ BS.Char8.takeWhile (/= '\r') value

writeLspMessage :: Handle -> BS.ByteString -> IO ()
writeLspMessage handle message = do
  BS.Char8.hPutStr handle $
    "Content-Length: "
      <> BS.Char8.pack (show $ BS.length message)
      <> "\r\n\r\n"
  BS.hPut handle message
  hFlush handle

applyEditRequestId :: BS.ByteString -> Maybe Aeson.Value
applyEditRequestId message = do
  Aeson.Object object <- Aeson.decodeStrict' message
  Aeson.String method <- KeyMap.lookup "method" object
  guard $ method == "workspace/applyEdit"
  KeyMap.lookup "id" object

rejectionResponse :: Aeson.Value -> BS.ByteString
rejectionResponse requestId =
  LBS.toStrict . Aeson.encode $
    Aeson.object
      [ "jsonrpc" Aeson..= ("2.0" :: Text),
        "id" Aeson..= requestId,
        "result"
          Aeson..= Aeson.object
            [ "applied" Aeson..= False,
              "failureReason" Aeson..= ("test rejection" :: Text)
            ]
      ]

withWikiFixture :: (WikiFixture -> IO a) -> IO a
withWikiFixture = bracket createFixture removeFixture

createFixture :: IO WikiFixture
createFixture = do
  temporary <- Directory.getTemporaryDirectory
  slug <- Slug.generateRandomSlug
  let root = temporary </> ("wiki-lsp-test-" <> unpack slug.text)
      state = root </> "state"
      source = root </> "source.md"
  Directory.createDirectory root
  Text.IO.writeFile source "# Source\n\n- Parent concept\n"
  pure WikiFixture {..}

removeFixture :: WikiFixture -> IO ()
removeFixture = Directory.removeDirectoryRecursive . (.root)

listMarkdownFiles :: FilePath -> IO [FilePath]
listMarkdownFiles root =
  sort . filter ((== ".md") . takeExtension)
    <$> Directory.listDirectory root

eventually :: IO Bool -> IO Bool
eventually check = go (50 :: Int)
  where
    go :: Int -> IO Bool
    go 0 = pure False
    go attempts = do
      success <- check
      if success
        then pure True
        else Concurrent.threadDelay 20_000 >> go (attempts - 1)
