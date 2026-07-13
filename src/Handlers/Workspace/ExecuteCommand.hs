module Handlers.Workspace.ExecuteCommand
  ( executeCommand,
    commandNames,
    spec_createNoteArgs,
    spec_rollbackMutation,
  )
where

import Data.Aeson qualified as Aeson
import Data.IxSet.Typed qualified as IxSet
import Data.Text qualified as Text
import Effectful.FileSystem
import Effectful.State.Static.Shared (State, get, modify)
import Handlers.Prelude
import LSP.Mutation
import Language.LSP.Protocol.Lens as J hiding (executeCommand, length, to)
import Language.LSP.Protocol.Types qualified as LSP
import Language.LSP.Server
  ( sendNotification,
    sendRequest,
  )
import Models.NoteInfo
import Models.NoteInfo.IO qualified as NoteInfo.IO
import Models.NoteInfo.Query qualified as Query
import Models.NoteCreation
import Models.Slug qualified as Slug
import MyPrelude
import Text.Pandoc.Definition

commandNames :: [Text]
commandNames =
  [ "wiki.nextDay",
    "wiki.prevDay",
    "wiki.today",
    "wiki.createNoteFromSelection"
  ]

executeCommand ::
  ( Logging :> es,
    State NoteInfoCache :> es,
    State MutationGate :> es,
    FileSystem :> es,
    VFSAccess :> es,
    Diagnostics :> es,
    LSP :> es,
    Concurrent :> es,
    IOE :> es
  ) =>
  HandlerFor 'Method_WorkspaceExecuteCommand es
executeCommand request = do
  let cmd = request ^. J.params . J.command
      args = request ^. J.params . J.arguments
  case cmd of
    "wiki.nextDay" -> navigateDay 1 args
    "wiki.prevDay" -> navigateDay (-1) args
    "wiki.today" -> navigateToday
    "wiki.createNoteFromSelection" -> createNoteFromSelection args
    _ -> do
      logWarn $ "Unknown command: " <> cmd
      pure $ InR LSP.Null

data CreateNoteArgs = CreateNoteArgs
  { textDocument :: Maybe TextDocumentIdentifier,
    uri :: Maybe Uri,
    range :: Maybe Range,
    openAfterCreation :: Maybe Bool,
    completionTitle :: Maybe Text,
    slug :: Maybe Slug.Slug
  }
  deriving (Generic)
  deriving (FromJSON) via FastGenericEncoding CreateNoteArgs

spec_createNoteArgs :: Spec
spec_createNoteArgs = describe "create-note command arguments" do
  it "accepts the standard text-document and range parameter shape" do
    let decoded =
          Aeson.eitherDecode
            "{\"textDocument\":{\"uri\":\"file:///wiki/source.md\"},\
            \\"range\":{\"start\":{\"line\":0,\"character\":1},\
            \\"end\":{\"line\":0,\"character\":4}}}" ::
            Either String CreateNoteArgs
    fmap (fmap (view J.uri) . (.textDocument)) decoded
      `shouldBe` Right (Just (LSP.Uri "file:///wiki/source.md"))

createNoteFromSelection ::
  ( Logging :> es,
    State NoteInfoCache :> es,
    State MutationGate :> es,
    FileSystem :> es,
    VFSAccess :> es,
    Diagnostics :> es,
    LSP :> es,
    Concurrent :> es,
    IOE :> es
  ) =>
  Maybe [Aeson.Value] ->
  Eff
    (Error (TResponseError 'Method_WorkspaceExecuteCommand) : es)
    (Aeson.Value |? LSP.Null)
createNoteFromSelection args = do
  parsedArgs <- parseCreateNoteArgs args
  case (parsedArgs.completionTitle, parsedArgs.slug) of
    (Just title, Just slug) -> createFromCompletion slug title
    _ -> createFromSelection parsedArgs
  pure $ InR LSP.Null

parseCreateNoteArgs ::
  (Error (TResponseError 'Method_WorkspaceExecuteCommand) :> es) =>
  Maybe [Aeson.Value] -> Eff es CreateNoteArgs
parseCreateNoteArgs (Just (value : _)) =
  case Aeson.fromJSON value of
    Aeson.Success parsed -> pure parsed
    Aeson.Error err ->
      throwCommandError $ "Invalid note creation arguments: " <> pack err
parseCreateNoteArgs _ = throwCommandError "Missing note creation arguments"

createFromCompletion ::
  ( Logging :> es,
    State NoteInfoCache :> es,
    State MutationGate :> es,
    Concurrent :> es,
    LSP :> es,
    FileSystem :> es,
    VFSAccess :> es,
    IOE :> es
  ) =>
  Slug.Slug ->
  Text ->
  Eff (Error (TResponseError 'Method_WorkspaceExecuteCommand) : es) ()
createFromCompletion slug titleMarkdown = do
  draft <- inferNoteDraft titleMarkdown `onLeft` throwCommandError
  result <- withMutationLease (creationDescription draft) \_lease -> do
    cache <- get
    case exactMatchCaseFold draft.title cache of
      Just existing -> void $ readCachedNote existing
      Nothing -> do
        void $ writeNewNote slug draft
        persistCacheStrictPreservingNote
  result `onLeft` throwMutationBlocked "create a note"

createFromSelection ::
  ( Logging :> es,
    State NoteInfoCache :> es,
    State MutationGate :> es,
    Concurrent :> es,
    FileSystem :> es,
    VFSAccess :> es,
    Diagnostics :> es,
    LSP :> es,
    IOE :> es
  ) =>
  CreateNoteArgs ->
  Eff (Error (TResponseError 'Method_WorkspaceExecuteCommand) : es) ()
createFromSelection args = do
  sourceUri <-
    ((args.textDocument <&> view J.uri) <|> args.uri)
      `onNothing` throwCommandError "Missing source textDocument"
  sourceRange <- args.range `onNothing` throwCommandError "Missing selection range"
  let normalizedUri = toNormalizedUri sourceUri
  selected <-
    getVirtualFileRange normalizedUri sourceRange
      `onNothingM` throwCommandError "Could not read the selected source text"
  draft <- inferNoteDraft selected `onLeft` throwCommandError
  sourceDocument <-
    getVersionedTextDoc $ TextDocumentIdentifier sourceUri
  result <- withMutationLease (creationDescription draft) \lease -> do
    cache <- get
    mutation <- case exactMatchCaseFold draft.title cache of
      Just existing -> prepareExistingNote existing draft
      Nothing -> do
        newSlug <- liftIO Slug.generateRandomSlug
        mutation <- writeNewNote newSlug draft
        persistCacheStrictOrRollback mutation
        pure mutation
    let replacement = replacementLink (mutationSlug mutation) draft
    edit <-
      noteWorkspaceEdit
        mutation
        sourceDocument
        sourceRange
        replacement
    void $
      sendMutatingRequest
        lease
        SMethod_WorkspaceApplyEdit
        (ApplyWorkspaceEditParams (Just "Create wiki note") edit)
        (handleApplyEditResponse mutation $ fromMaybe True args.openAfterCreation)
  result `onLeft` throwMutationBlocked "create a note"

data NoteMutation
  = CreatedNote NoteInfo Text
  | UpdatedNote NoteInfo Text Text (Maybe Int32)
  | ReusedNote NoteInfo

mutationSlug :: NoteMutation -> Slug.Slug
mutationSlug = \case
  CreatedNote note _ -> note.slug
  UpdatedNote note _ _ _ -> note.slug
  ReusedNote note -> note.slug

mutationNote :: NoteMutation -> NoteInfo
mutationNote = \case
  CreatedNote note _ -> note
  UpdatedNote note _ _ _ -> note
  ReusedNote note -> note

exactMatchCaseFold :: Text -> NoteInfoCache -> Maybe NoteInfo
exactMatchCaseFold title =
  find (\note -> Text.toCaseFold note.title == Text.toCaseFold title)
    . IxSet.toList

writeNewNote ::
  (FileSystem :> es, IOE :> es, State NoteInfoCache :> es, Logging :> es) =>
  Slug.Slug -> NoteDraft -> Eff es NoteMutation
writeNewNote slug draft = do
  now <- liftIO getZonedTime
  let contents = renderNote now draft
      path = Slug.intoFilePathRelativeToDir "." slug.text
  writeFileUtf8 path contents
  let note = NoteInfo slug draft.title Nothing
  modify $ IxSet.insert note
  -- Keeping the complete staged contents makes rollback data-safe. If pending
  -- notes ever become large enough for this to matter, a digest is sufficient.
  pure $ CreatedNote note contents

prepareExistingNote ::
  ( FileSystem :> es,
    VFSAccess :> es,
    Logging :> es,
    Diagnostics :> es,
    Error (TResponseError 'Method_WorkspaceExecuteCommand) :> es
  ) =>
  NoteInfo -> NoteDraft -> Eff es NoteMutation
prepareExistingNote note draft = do
  (uri, version, original) <- readCachedNote note
  if isNothing draft.body && isNothing draft.publicAlternate
    then pure $ ReusedNote note
    else prepareContents uri version original
  where
    prepareContents uri version original
      | isNothing draft.publicAlternate = collision
      | Just alternate <- draft.publicAlternate = do
          parsed <- parseDocumentThrow uri original
          let existingAlternate = publicAlternateFromPage parsed
          when (maybe False (/= alternate) existingAlternate) $
            throwCommandError "Existing note has a different public_alternate"
          when (isJust draft.body && not (isStub parsed)) $
            throwCommandError collisionMessage
          let withAlternate =
                if isNothing existingAlternate
                  then addPublicAlternate alternate original
                  else original
              updated =
                maybe withAlternate (appendBody withAlternate) draft.body
          if updated == original
            then pure $ ReusedNote note
            else pure $ UpdatedNote note original updated version
      | otherwise = collision

    collision =
      throwCommandError collisionMessage
    collisionMessage =
      "A note titled \x201c" <> draft.title <> "\x201d already exists and has content"

readCachedNote ::
  ( FileSystem :> es,
    VFSAccess :> es,
    Logging :> es,
    Error (TResponseError 'Method_WorkspaceExecuteCommand) :> es
  ) =>
  NoteInfo -> Eff es (NormalizedUri, Maybe Int32, Text)
readCachedNote note = do
  cwd <- getCurrentDirectory
  let uri = Slug.intoUri cwd note.slug
  (version, contents) <- tryGetUriContents uri
  contents' <-
    contents
      `onNothing` throwCommandError
        "The note cache refers to a note that no longer exists"
  pure (uri, version, contents')

publicAlternateFromPage :: Pandoc -> Maybe Text
publicAlternateFromPage (Pandoc (Meta metadata) _) =
  metadata ^? ix "public_alternate" . #_MetaString

addPublicAlternate :: Text -> Text -> Text
addPublicAlternate alternate contents =
  case lines contents of
    "---" : rest ->
      case break (== "---") rest of
        (frontmatter, closing : body) ->
          unlines $
            "---"
              : frontmatter
                <> ["public_alternate: " <> alternate, closing]
                <> body
        _ -> prepend
    _ -> prepend
  where
    prepend = unlines ["---", "public_alternate: " <> alternate, "---", ""] <> contents

appendBody :: Text -> Text -> Text
appendBody contents body = Text.stripEnd contents <> "\n" <> body

creationDescription :: NoteDraft -> Text
creationDescription draft = "creating note “" <> draft.title <> "”"

throwMutationBlocked ::
  (Error (TResponseError 'Method_WorkspaceExecuteCommand) :> es) =>
  Text -> PendingMutation -> Eff es a
throwMutationBlocked requested =
  throwCommandError . mutationBlockedMessage requested

noteWorkspaceEdit ::
  ( FileSystem :> es,
    Error (TResponseError 'Method_WorkspaceExecuteCommand) :> es
  ) =>
  NoteMutation ->
  VersionedTextDocumentIdentifier ->
  Range ->
  Text ->
  Eff es WorkspaceEdit
noteWorkspaceEdit mutation sourceDocument sourceRange replacement = do
  targetChanges <- case mutation of
    UpdatedNote note original updated version -> do
      cwd <- getCurrentDirectory
      let targetUri = fromNormalizedUri $ Slug.intoUri cwd note.slug
          targetLine = fromIntegral (length (lines original) + 1)
          targetRange = Range (Position 0 0) (Position targetLine 0)
      when (targetUri == sourceDocument._uri) $
        throwCommandError $
          "Cannot update public_alternate while extracting from the same note"
      pure
        [ documentChange
            targetUri
            (maybe (InR LSP.Null) InL version)
            [TextEdit targetRange updated]
        ]
    _ -> pure []
  let sourceChange =
        documentChange
          sourceDocument._uri
          (InL sourceDocument._version)
          [TextEdit sourceRange replacement]
  pure $
    WorkspaceEdit
      { _changes = Nothing,
        -- Target-first ordering preserves copied content if a client cannot
        -- apply a multi-document edit transactionally.
        _documentChanges = Just $ targetChanges <> [sourceChange],
        _changeAnnotations = Nothing
      }
  where
    documentChange uri version edits =
      InL $
        TextDocumentEdit
          (OptionalVersionedTextDocumentIdentifier uri version)
          (map InL edits)

handleApplyEditResponse ::
  ( Logging :> es,
    State NoteInfoCache :> es,
    FileSystem :> es,
    LSP :> es,
    IOE :> es
  ) =>
  NoteMutation ->
  Bool ->
  Either
    (TResponseError 'Method_WorkspaceApplyEdit)
    ApplyWorkspaceEditResult ->
  Eff es ()
handleApplyEditResponse mutation openAfterCreation = \case
  Left err -> applyFailed $ "The editor failed to apply the wiki edit: " <> err._message
  Right result
    | result._applied ->
        when openAfterCreation $ openDocument (mutationNote mutation)
    | otherwise ->
        applyFailed $
          fromMaybe "The editor rejected the wiki edit" result._failureReason
  where
    applyFailed message = do
      rollbackMutation mutation
      logError message
      showErrorMessage message

persistCacheStrictOrRollback ::
  ( Logging :> es,
    State NoteInfoCache :> es,
    FileSystem :> es,
    LSP :> es,
    IOE :> es,
    Error (TResponseError 'Method_WorkspaceExecuteCommand) :> es
  ) =>
  NoteMutation -> Eff es ()
persistCacheStrictOrRollback mutation = do
  cache <- get
  NoteInfo.IO.saveCacheStrict cache `catchAny` \exception -> do
    rollbackMutation mutation
    throwCommandError $
      "Could not persist the note cache: " <> tshow exception

-- Completion commands run after their text edit has already been inserted.
-- Preserve the created target if persistence fails rather than leave that link
-- broken; the in-memory cache remains usable and a later scan can persist it.
persistCacheStrictPreservingNote ::
  ( State NoteInfoCache :> es,
    IOE :> es,
    Error (TResponseError 'Method_WorkspaceExecuteCommand) :> es
  ) =>
  Eff es ()
persistCacheStrictPreservingNote = do
  cache <- get
  NoteInfo.IO.saveCacheStrict cache `catchAny` \exception ->
    throwCommandError $
      "Created the note but could not persist the note cache: "
        <> tshow exception

rollbackMutation ::
  ( Logging :> es,
    FileSystem :> es,
    State NoteInfoCache :> es,
    LSP :> es,
    IOE :> es
  ) =>
  NoteMutation -> Eff es ()
rollbackMutation = \case
  CreatedNote note stagedContents -> do
    let path = Slug.intoFilePathRelativeToDir "." note.slug.text
    exists <- doesFileExist path
    contents <-
      if exists
        then Just <$> try @_ @SomeException (readFileUtf8 path)
        else pure Nothing
    case rollbackDisposition stagedContents contents of
      FileMissing -> removeCacheEntry note.slug
      FileUnchanged -> do
        removeFile path
        removeCacheEntry note.slug
      FileChanged -> preserveModifiedNote note
      FileUnverifiable exception -> do
        logWarn $
          "Could not verify staged note before rollback: "
            <> tshow exception
        preserveModifiedNote note
  UpdatedNote {} -> pure ()
  ReusedNote _ -> pure ()
  where
    removeCacheEntry slug = do
      modify $ IxSet.deleteIx slug
      get >>= NoteInfo.IO.saveCache

    preserveModifiedNote note = do
      let message =
            "The editor edit failed, but modified note “"
              <> note.title
              <> "” was preserved"
      logWarn message
      showWarning message

data RollbackDisposition
  = FileMissing
  | FileUnchanged
  | FileChanged
  | FileUnverifiable SomeException
  deriving stock (Show)

rollbackDisposition ::
  Text -> Maybe (Either SomeException Text) -> RollbackDisposition
rollbackDisposition _ Nothing = FileMissing
rollbackDisposition staged (Just (Right current))
  | staged == current = FileUnchanged
  | otherwise = FileChanged
rollbackDisposition _ (Just (Left exception)) = FileUnverifiable exception

spec_rollbackMutation :: Spec
spec_rollbackMutation = describe "mutation rollback" do
  it "deletes only the exact staged contents" do
    rollbackDisposition "staged" (Just $ Right "staged")
      `shouldSatisfy` \case
        FileUnchanged -> True
        _ -> False
    rollbackDisposition "staged" (Just $ Right "user edit")
      `shouldSatisfy` \case
        FileChanged -> True
        _ -> False

-- | Navigate to today's date note.
navigateToday ::
  ( Logging :> es,
    State NoteInfoCache :> es,
    State MutationGate :> es,
    FileSystem :> es,
    LSP :> es,
    IOE :> es
  ) =>
  Eff
    (Error (TResponseError 'Method_WorkspaceExecuteCommand) : es)
    (Aeson.Value |? LSP.Null)
navigateToday = do
  today <- liftIO $ localDay . zonedTimeToLocalTime <$> getZonedTime
  cache <- get
  target <- case Query.notesForDay today cache of
    (note : _) -> pure note
    [] -> do
      result <- withMutationLease "creating today's note" \_lease -> do
        cache' <- get
        case Query.notesForDay today cache' of
          (note : _) -> pure note
          [] -> do
            (note, contents) <- NoteInfo.IO.createDateNote today
            modify $ IxSet.insert note
            persistCacheStrictOrRollback $ CreatedNote note contents
            pure note
      result `onLeft` throwMutationBlocked "create today's note"
  openDocument target
  pure $ InR LSP.Null

-- | Navigate to the note for a day offset from the current
-- note's day.
navigateDay ::
  ( Logging :> es,
    State NoteInfoCache :> es,
    FileSystem :> es,
    LSP :> es,
    IOE :> es,
    Error (TResponseError 'Method_WorkspaceExecuteCommand)
      :> es
  ) =>
  Integer ->
  Maybe [Aeson.Value] ->
  Eff
    es
    (Aeson.Value |? LSP.Null)
navigateDay offset args = do
  uri <- extractUriArg args
  let nuri = toNormalizedUri uri
  slug <-
    onNothing
      (nuriToSlug nuri)
      (throwCommandError "Could not extract slug from URI")
  cache <- get
  note <-
    onNothing
      (Query.noteForSlug slug cache)
      (throwCommandError "Note not found in cache")
  day <-
    onNothing
      note.day
      ( do
          showWarning "Current note is not a date note"
          throwCommandError "Current note is not a date note"
      )
  let targetDay = addDays offset day
      targetNotes = Query.notesForDay targetDay cache
  case targetNotes of
    (target : _) -> do
      openDocument target
      pure $ InR LSP.Null
    [] -> do
      showWarning $
        "No note found for "
          <> pack
            ( formatTime
                defaultTimeLocale
                "%Y-%m-%d"
                targetDay
            )
      pure $ InR LSP.Null

extractUriArg ::
  ( Error
      (TResponseError 'Method_WorkspaceExecuteCommand)
      :> es
  ) =>
  Maybe [Aeson.Value] ->
  Eff es Uri
extractUriArg (Just (arg : _)) =
  case Aeson.fromJSON arg of
    Aeson.Success uri -> pure uri
    Aeson.Error _ ->
      throwCommandError "Invalid URI argument"
extractUriArg _ =
  throwCommandError "Missing URI argument"

nuriToSlug :: NormalizedUri -> Maybe Slug.Slug
nuriToSlug nuri = do
  fp <- nuriToFilePath nuri
  Slug.fromMarkdownFilePath fp

openDocument ::
  (LSP :> es, IOE :> es, FileSystem :> es, Logging :> es) =>
  NoteInfo -> Eff es ()
openDocument note = do
  cwd <- getCurrentDirectory
  let uri = Slug.intoUri cwd note.slug
  void $
    -- The response can arrive after this command handler returns, so retain a
    -- fork-safe effect environment for the callback.
    withUnliftStrategy SeqForkUnlift $
      sendRequest
        SMethod_WindowShowDocument
        ShowDocumentParams
          { _uri = fromNormalizedUri uri,
            _external = Nothing,
            _takeFocus = Just True,
            _selection = Nothing
          }
        (\case
          Left err -> navigationFailed err._message
          Right result ->
            unless result._success $
              navigationFailed "The editor could not open the wiki note"
        )
  where
    navigationFailed message = do
      logError message
      showErrorMessage message

showWarning :: (MonadLsp config m) => Text -> m ()
showWarning msg =
  sendNotification
    SMethod_WindowShowMessage
    ShowMessageParams
      { _type_ = MessageType_Warning,
        _message = msg
      }

showErrorMessage :: (MonadLsp config m) => Text -> m ()
showErrorMessage msg =
  sendNotification
    SMethod_WindowShowMessage
    ShowMessageParams
      { _type_ = MessageType_Error,
        _message = msg
      }

throwCommandError ::
  ( Error
      (TResponseError 'Method_WorkspaceExecuteCommand)
      :> es
  ) =>
  Text ->
  Eff es a
throwCommandError msg =
  throwError_ $
    TResponseError
      { _code = InL LSPErrorCodes_RequestFailed,
        _message = msg,
        _xdata = Nothing
      }
