module Handlers.Workspace.ExecuteCommand
  ( executeCommand,
    commandNames,
    spec_createNoteArgs,
  )
where

import Data.Aeson qualified as Aeson
import Data.IxSet.Typed qualified as IxSet
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Effectful.FileSystem
import Effectful.State.Static.Shared (State, get, modify)
import Handlers.Prelude
import Language.LSP.Protocol.Lens as J hiding (executeCommand, length, to)
import Language.LSP.Protocol.Types qualified as LSP
import Language.LSP.Server (sendNotification, sendRequest)
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
    FileSystem :> es,
    VFSAccess :> es,
    Diagnostics :> es,
    LSP :> es,
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
    FileSystem :> es,
    VFSAccess :> es,
    Diagnostics :> es,
    LSP :> es,
    IOE :> es,
    Error (TResponseError 'Method_WorkspaceExecuteCommand) :> es
  ) =>
  Maybe [Aeson.Value] ->
  Eff es (Aeson.Value |? LSP.Null)
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
    Aeson.Error err -> throwCommandError $ "Invalid note creation arguments: " <> pack err
parseCreateNoteArgs _ = throwCommandError "Missing note creation arguments"

createFromCompletion ::
  ( Logging :> es,
    State NoteInfoCache :> es,
    FileSystem :> es,
    IOE :> es,
    Error (TResponseError 'Method_WorkspaceExecuteCommand) :> es
  ) =>
  Slug.Slug -> Text -> Eff es ()
createFromCompletion slug titleMarkdown = do
  draft <- inferNoteDraft titleMarkdown `onLeft` throwCommandError
  cache <- get
  case exactMatchCaseFold draft.title cache of
    Just _ -> pure ()
    Nothing -> void $ writeNewNote slug draft

createFromSelection ::
  ( Logging :> es,
    State NoteInfoCache :> es,
    FileSystem :> es,
    VFSAccess :> es,
    Diagnostics :> es,
    LSP :> es,
    IOE :> es,
    Error (TResponseError 'Method_WorkspaceExecuteCommand) :> es
  ) =>
  CreateNoteArgs -> Eff es ()
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
  cache <- get
  mutation <- case exactMatchCaseFold draft.title cache of
    Just existing -> prepareExistingNote existing draft
    Nothing -> do
      newSlug <- liftIO Slug.generateRandomSlug
      void $ writeNewNote newSlug draft
      pure $ CreatedNote newSlug
  let targetSlug = mutationSlug mutation
      replacement = replacementLink targetSlug draft
  applied <- applyNoteEdits mutation sourceUri sourceRange replacement
  unless applied do
    rollbackMutation mutation
    throwCommandError "The editor rejected the source note edit; note creation was rolled back"
  when (fromMaybe True args.openAfterCreation) do
    cache' <- get
    withJust (Query.noteForSlug targetSlug cache') openDocument

data NoteMutation
  = CreatedNote Slug.Slug
  | UpdatedNote NoteInfo Text Text
  | ReusedNote NoteInfo

mutationSlug :: NoteMutation -> Slug.Slug
mutationSlug = \case
  CreatedNote slug -> slug
  UpdatedNote note _ _ -> note.slug
  ReusedNote note -> note.slug

exactMatchCaseFold :: Text -> NoteInfoCache -> Maybe NoteInfo
exactMatchCaseFold title =
  find (\note -> Text.toCaseFold note.title == Text.toCaseFold title)
    . IxSet.toList

writeNewNote ::
  (FileSystem :> es, IOE :> es, State NoteInfoCache :> es, Logging :> es) =>
  Slug.Slug -> NoteDraft -> Eff es Text
writeNewNote slug draft = do
  now <- liftIO getZonedTime
  let contents = renderNote now draft
      path = Slug.intoFilePathRelativeToDir "." slug.text
  writeFileUtf8 path contents
  let note = NoteInfo slug draft.title Nothing
  modify $ IxSet.insert note
  pure contents

prepareExistingNote ::
  ( FileSystem :> es,
    VFSAccess :> es,
    Logging :> es,
    Diagnostics :> es,
    Error (TResponseError 'Method_WorkspaceExecuteCommand) :> es
  ) =>
  NoteInfo -> NoteDraft -> Eff es NoteMutation
prepareExistingNote note draft
  | isNothing draft.body && isNothing draft.publicAlternate = pure $ ReusedNote note
  | isNothing draft.publicAlternate = collision
  | Just alternate <- draft.publicAlternate = do
      cwd <- getCurrentDirectory
      let uri = Slug.intoUri cwd note.slug
      (_, mContents) <- tryGetUriContents uri
      original <- mContents `onNothing` throwCommandError "Could not read the existing note"
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
          updated = maybe withAlternate (appendBody withAlternate) draft.body
      if updated == original
        then pure $ ReusedNote note
        else pure $ UpdatedNote note original updated
  | otherwise = collision
  where
    collision =
      throwCommandError collisionMessage
    collisionMessage =
      "A note titled \x201c" <> draft.title <> "\x201d already exists and has content"

publicAlternateFromPage :: Pandoc -> Maybe Text
publicAlternateFromPage (Pandoc (Meta metadata) _) =
  metadata ^? ix "public_alternate" . #_MetaString

addPublicAlternate :: Text -> Text -> Text
addPublicAlternate alternate contents =
  case lines contents of
    "---" : rest ->
      case break (== "---") rest of
        (frontmatter, closing : body) ->
          unlines $ "---" : frontmatter <> ["public_alternate: " <> alternate, closing] <> body
        _ -> prepend
    _ -> prepend
  where
    prepend = unlines ["---", "public_alternate: " <> alternate, "---", ""] <> contents

appendBody :: Text -> Text -> Text
appendBody contents body = Text.stripEnd contents <> "\n" <> body

applyNoteEdits ::
  ( LSP :> es,
    IOE :> es,
    FileSystem :> es,
    Error (TResponseError 'Method_WorkspaceExecuteCommand) :> es
  ) =>
  NoteMutation -> Uri -> Range -> Text -> Eff es Bool
applyNoteEdits mutation sourceUri sourceRange replacement = do
  result <- liftIO newEmptyMVar
  targetChanges <- case mutation of
    UpdatedNote note original updated -> do
      cwd <- getCurrentDirectory
      let targetUri = fromNormalizedUri $ Slug.intoUri cwd note.slug
          targetLine = fromIntegral (length (lines original) + 1)
          targetRange = Range (Position 0 0) (Position targetLine 0)
      when (targetUri == sourceUri) $
        throwCommandError "Cannot update public_alternate while extracting from the same note"
      pure [(targetUri, [TextEdit targetRange updated])]
    _ -> pure []
  let changes =
        Map.fromListWith (<>) $
          (sourceUri, [TextEdit sourceRange replacement]) : targetChanges
  let edit =
        WorkspaceEdit
          { _changes = Just changes,
            _documentChanges = Nothing,
            _changeAnnotations = Nothing
          }
  void $
    sendRequest
      SMethod_WorkspaceApplyEdit
      (ApplyWorkspaceEditParams (Just "Create wiki note") edit)
      (liftIO . putMVar result)
  response <- liftIO $ takeMVar result
  pure $ either (const False) (._applied) response

rollbackMutation ::
  (FileSystem :> es, State NoteInfoCache :> es) => NoteMutation -> Eff es ()
rollbackMutation = \case
  CreatedNote slug -> do
    removeFile $ Slug.intoFilePathRelativeToDir "." slug.text
    modify $ IxSet.deleteIx slug
  UpdatedNote {} -> pure ()
  ReusedNote _ -> pure ()

-- | Navigate to today's date note.
navigateToday ::
  ( Logging :> es,
    State NoteInfoCache :> es,
    FileSystem :> es,
    LSP :> es,
    IOE :> es,
    Error (TResponseError 'Method_WorkspaceExecuteCommand)
      :> es
  ) =>
  Eff es (Aeson.Value |? LSP.Null)
navigateToday = do
  today <- liftIO $ localDay . zonedTimeToLocalTime <$> getZonedTime
  cache <- get
  target <- case Query.notesForDay today cache of
    (note : _) -> pure note
    [] -> do
      note <- NoteInfo.IO.createDateNote today
      modify $ IxSet.insert note
      pure note
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
  (LSP :> es, IOE :> es, FileSystem :> es) =>
  NoteInfo -> Eff es ()
openDocument note = do
  cwd <- getCurrentDirectory
  let uri = Slug.intoUri cwd note.slug
  void $
    sendRequest
      SMethod_WindowShowDocument
      ShowDocumentParams
        { _uri = fromNormalizedUri uri,
          _external = Nothing,
          _takeFocus = Just True,
          _selection = Nothing
        }
      (const $ pure ())

showWarning :: (MonadLsp config m) => Text -> m ()
showWarning msg =
  sendNotification
    SMethod_WindowShowMessage
    ShowMessageParams
      { _type_ = MessageType_Warning,
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
