module LSP.Mutation
  ( MutationGate,
    MutationLease,
    PendingMutation (..),
    initialMutationGate,
    withMutationLease,
    sendMutatingRequest,
    mutationBlockedMessage,
    spec_mutationGate,
  )
where

import Effectful.State.Static.Shared (State)
import Effectful.State.Static.Shared qualified as State
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Language.LSP.Server (sendNotification, sendRequest)
import LSP.Raw
import MyPrelude
import Utils.Logging

newtype MutationToken = MutationToken Word64
  deriving stock (Show, Eq, Ord)

data MutationPhase
  = Preparing
  | AwaitingResponse
  | Completing
  deriving stock (Show, Eq)

data PendingMutation = PendingMutation
  { description :: Text,
    startedAt :: UTCTime,
    phase :: MutationPhase,
    token :: MutationToken,
    warned :: Bool
  }
  deriving stock (Show, Eq)

data MutationGate = MutationGate
  { nextToken :: Word64,
    pending :: Maybe PendingMutation
  }
  deriving stock (Show, Eq)

initialMutationGate :: MutationGate
initialMutationGate = MutationGate 0 Nothing

data MutationLease = MutationLease
  { token :: MutationToken,
    description :: Text,
    handedToCallback :: IORef Bool
  }

mutationBlockedMessage :: Text -> PendingMutation -> Text
mutationBlockedMessage requested blocker =
  "Cannot "
    <> requested
    <> " while the editor is still applying “"
    <> blocker.description
    <> "”"

tryAcquireMutation ::
  (State MutationGate :> es, IOE :> es) =>
  Text -> Eff es (Either PendingMutation MutationToken)
tryAcquireMutation description = do
  startedAt <- liftIO getCurrentTime
  State.state \gate -> case gate.pending of
    Just blocker -> (Left blocker, gate)
    Nothing ->
      let token = MutationToken gate.nextToken
          pending = PendingMutation {phase = Preparing, warned = False, ..}
       in ( Right token,
            gate
              { nextToken = gate.nextToken + 1,
                pending = Just pending
              }
          )

transitionMutation ::
  (State MutationGate :> es) =>
  MutationToken -> MutationPhase -> MutationPhase -> Eff es Bool
transitionMutation token from to =
  State.state \gate -> case gate.pending of
    Just pending
      | pending.token == token && pending.phase == from ->
          (True, gate {pending = Just pending {phase = to}})
    _ -> (False, gate)

releaseMutation ::
  (State MutationGate :> es) => MutationToken -> Eff es Bool
releaseMutation token =
  State.state \gate -> case gate.pending of
    Just pending
      | pending.token == token -> (True, gate {pending = Nothing})
    _ -> (False, gate)

-- | Acquire the mutation gate for preparation. If the action hands its lease
-- to 'sendMutatingRequest', the response callback owns finalization. Otherwise
-- the gate is released when the lexical action finishes or fails.
withMutationLease ::
  forall e es a.
  (State MutationGate :> es, IOE :> es) =>
  Text ->
  (MutationLease -> Eff (Error e : es) a) ->
  Eff (Error e : es) (Either PendingMutation a)
withMutationLease description action = do
  acquired <- raise $ tryAcquireMutation description
  case acquired of
    Left blocker -> pure $ Left blocker
    Right token -> do
      handedToCallback <- liftIO $ newIORef False
      let lease = MutationLease {..}
          releaseUnlessHandedOff = do
            handedOff <- liftIO $ readIORef handedToCallback
            unless handedOff $ void . raise $ releaseMutation token
          rethrow _ err = do
            releaseUnlessHandedOff
            throwError_ err
      result <-
        (action lease `catchError` rethrow)
          `onException` releaseUnlessHandedOff
      releaseUnlessHandedOff
      pure $ Right result

sendMutatingRequest ::
  forall e m es.
  ( LSP :> es,
    State MutationGate :> es,
    Concurrent :> es,
    Logging :> es,
    IOE :> es
  ) =>
  MutationLease ->
  SServerMethod m ->
  MessageParams m ->
  (Either (TResponseError m) (MessageResult m) -> Eff es ()) ->
  Eff (Error e : es) (LspId m)
sendMutatingRequest lease method params callback = do
  awaiting <-
    raise $
      transitionMutation lease.token Preparing AwaitingResponse
  unless awaiting $ throwIO $ userError "Mutation lease is no longer preparing"
  requestId <-
    -- LSP retains the response callback after the command handler returns.
    -- Clone the effect environment so that eventual invocation remains valid.
    withUnliftStrategy SeqForkUnlift $
      sendRequest method params $ \response ->
        raise $ runCompletion lease.token response
  liftIO $ writeIORef lease.handedToCallback True
  raise $ startWatchdog lease.token
  pure requestId
  where
    runCompletion token response = do
      completed <-
        runMutationCompletion
          token
          (callback response)
          reportCallbackException
      unless completed $
        logWarn $
          "Ignored duplicate or stale response for mutation "
            <> tshow token

    reportCallbackException exception = do
      let message =
            "Failed while finalizing “"
              <> lease.description
              <> "”: "
              <> tshow exception
      logError message
      sendNotification
        SMethod_WindowShowMessage
        (ShowMessageParams MessageType_Error message)

startWatchdog ::
  ( LSP :> es,
    State MutationGate :> es,
    Concurrent :> es,
    Logging :> es,
    IOE :> es
  ) =>
  MutationToken -> Eff es ()
startWatchdog token = void $ forkIO do
  sleep 5
  warning <- claimWatchdogWarning token
  withJust warning \message -> do
    logWarn message
    sendNotification
      SMethod_WindowShowMessage
      (ShowMessageParams MessageType_Warning message)

runMutationCompletion ::
  (State MutationGate :> es, IOE :> es) =>
  MutationToken ->
  Eff es () ->
  (SomeException -> Eff es ()) ->
  Eff es Bool
runMutationCompletion token callback reportException = do
  claimed <- transitionMutation token AwaitingResponse Completing
  when claimed $
    (callback `catchAny` reportException)
      `finally` void (releaseMutation token)
  pure claimed

claimWatchdogWarning ::
  (State MutationGate :> es) =>
  MutationToken ->
  Eff es (Maybe Text)
claimWatchdogWarning token =
  State.state \gate -> case gate.pending of
    Just pending
      | pending.token == token,
        pending.phase == AwaitingResponse,
        not pending.warned ->
          let message =
                "The editor has not responded while "
                  <> pending.description
           in ( Just message,
                gate {pending = Just pending {warned = True}}
              )
    _ -> (Nothing, gate)

spec_mutationGate :: Spec
spec_mutationGate = describe "mutation gate" do
  it "refuses a second mutation while one is preparing" do
    (result, _gate) <- runEff $ State.runState initialMutationGate do
      first <- tryAcquireMutation "first mutation"
      second <- tryAcquireMutation "second mutation"
      pure (has _Right first, has _Left second)
    result `shouldBe` (True, True)

  it "requires the current token to complete and release" do
    (result, gate) <- runEff $ State.runState initialMutationGate do
      acquired <- tryAcquireMutation "mutation"
      let token = either (error . show) id acquired
      awaiting <- transitionMutation token Preparing AwaitingResponse
      completing <- transitionMutation token AwaitingResponse Completing
      released <- releaseMutation token
      pure (awaiting, completing, released)
    result `shouldBe` (True, True, True)
    gate.pending `shouldBe` Nothing

  it "releases the gate when callback finalization throws" do
    (result, gate) <- runEff $ State.runState initialMutationGate do
      token <-
        tryAcquireMutation "mutation"
          <&> either (error . show) id
      void $ transitionMutation token Preparing AwaitingResponse
      reported <- liftIO $ newIORef False
      claimed <-
        runMutationCompletion
          token
          (throwIO $ userError "callback failed")
          (\_ -> liftIO $ writeIORef reported True)
      (claimed,) <$> liftIO (readIORef reported)
    result `shouldBe` (True, True)
    gate.pending `shouldBe` Nothing

  it "only claims one watchdog warning and keeps the mutation pending" do
    (warnings, gate) <- runEff $ State.runState initialMutationGate do
      token <-
        tryAcquireMutation "mutation"
          <&> either (error . show) id
      void $ transitionMutation token Preparing AwaitingResponse
      (,) <$> claimWatchdogWarning token <*> claimWatchdogWarning token
    warnings `shouldBe`
      (Just "The editor has not responded while mutation", Nothing)
    (gate.pending <&> (.warned)) `shouldBe` Just True
