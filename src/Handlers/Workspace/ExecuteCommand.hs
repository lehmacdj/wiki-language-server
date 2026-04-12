module Handlers.Workspace.ExecuteCommand
  ( executeCommand,
    commandNames,
  )
where

import Data.Aeson qualified as Aeson
import Effectful.State.Static.Shared (State, get)
import Handlers.Prelude
import Language.LSP.Protocol.Lens as J hiding (executeCommand, to)
import Language.LSP.Protocol.Types qualified as LSP
import Language.LSP.Server (sendNotification, sendRequest)
import Models.NoteInfo
import Models.NoteInfo.Query qualified as Query
import Models.Slug qualified as Slug
import MyPrelude

commandNames :: [Text]
commandNames =
  [ "wiki.nextDay",
    "wiki.prevDay"
  ]

executeCommand ::
  ( Logging :> es,
    State NoteInfoCache :> es,
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
    _ -> do
      logWarn $ "Unknown command: " <> cmd
      pure $ InR LSP.Null

-- | Navigate to the note for a day offset from the current
-- note's day.
navigateDay ::
  ( Logging :> es,
    State NoteInfoCache :> es,
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
  (MonadLsp config m) =>
  NoteInfo -> m ()
openDocument note = do
  let uri = Slug.intoUri "." note.slug
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
