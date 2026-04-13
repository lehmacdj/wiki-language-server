module Executable.WikiNote (wikiNoteMain) where

import Data.ByteString qualified as BS
import Data.IxSet.Typed qualified as IxSet
import Data.Text.Encoding qualified as Text
import Effectful.FileSystem (runFileSystem)
import Executable.Options (LookupMode (..), OutputFormat (..))
import LSP.VFS (runVFSAccessPure)
import Models.NoteInfo
import Models.NoteInfo.IO
  ( createDateNote,
    loadCache,
    rescanCache,
    saveCache,
  )
import Models.NoteInfo.Query qualified as Query
import Models.Slug (Slug (..))
import Models.Slug qualified as Slug
import MyPrelude
import System.Directory (setCurrentDirectory)
import System.Exit (exitFailure)
import System.IO (hPutStrLn)
import Utils.DateTimeParsing (NaturalLanguageParseError)
import Utils.DateTimeParsing qualified
import Utils.Diagnostics (runDiagnosticsNoOp)
import Utils.Logging (runLoggingNoOp)

wikiNoteMain ::
  LookupMode -> OutputFormat -> Bool -> FilePath -> IO ()
wikiNoteMain lookupMode outputFormat createIfMissing wikiDir = do
  setCurrentDirectory wikiDir
  cache <- runEffects loadCache
  case lookupMode of
    MatchingDate dateText -> handleDateLookup cache dateText
    MatchingTitle titleText ->
      handleTitleLookup cache titleText
    NewWithTitle titleText ->
      handleNewWithTitle cache titleText
  where
    handleDateLookup cache dateText = do
      now <- getZonedTime
      case parseDay now dateText of
        Left err -> do
          hPutStrLn stderr $
            "Failed to parse date: " <> show err
          exitFailure
        Right day ->
          lookupWithRescan
            cache
            (Query.notesForDay day)
            (runEffects $ createDateNote day)

    handleTitleLookup cache titleText =
      lookupWithRescan
        cache
        (fuzzyMatchByTitle titleText)
        (createTitleNote titleText)

    handleNewWithTitle cache titleText = do
      -- Check cache first, then rescan to be sure
      case exactMatchByTitle titleText cache of
        Just note -> alreadyExistsError note
        Nothing -> do
          cache' <- runEffects rescanCache
          case exactMatchByTitle titleText cache' of
            Just note -> alreadyExistsError note
            Nothing -> do
              note <- createTitleNote titleText
              runEffects $
                saveCache (IxSet.insert note cache')
              outputNote note
      where
        alreadyExistsError note = do
          let path =
                Slug.intoFilePathRelativeToDir
                  "."
                  note.slug.text
          hPutStrLn stderr $
            "Error: a note with this exact title "
              <> "already exists: "
              <> path
          exitFailure

    -- \| Look up notes in the cache, rescan on miss, and
    -- optionally create if still missing.
    lookupWithRescan ::
      NoteInfoCache ->
      (NoteInfoCache -> [NoteInfo]) ->
      IO NoteInfo ->
      IO ()
    lookupWithRescan cache search create =
      case search cache of
        (_ : _) -> outputNotes (search cache)
        [] -> do
          cache' <- runEffects rescanCache
          case search cache' of
            (_ : _) -> outputNotes (search cache')
            [] | createIfMissing -> do
              note <- create
              runEffects $
                saveCache (IxSet.insert note cache')
              outputNote note
            [] -> exitFailure

    runEffects =
      runEff
        . runFileSystem
        . runLoggingNoOp
        . runDiagnosticsNoOp
        . runVFSAccessPure

    outputNote :: NoteInfo -> IO ()
    outputNote note = case outputFormat of
      OutputPath ->
        putStrLn $
          pack $
            Slug.intoFilePathRelativeToDir "." note.slug.text
      OutputSlug -> putStrLn note.slug.text

    outputNotes :: [NoteInfo] -> IO ()
    outputNotes = mapM_ outputNote

    parseDay ::
      ZonedTime ->
      Text ->
      Either NaturalLanguageParseError Day
    parseDay = Utils.DateTimeParsing.parseDay

    createTitleNote :: Text -> IO NoteInfo
    createTitleNote title = do
      slug <- Slug.generateRandomSlug
      now <- getZonedTime
      let dateStr =
            formatTime
              defaultTimeLocale
              "%Y-%m-%dT%H:%M:%S%Ez"
              now
          content =
            unlines
              [ "---",
                "date: " <> pack dateStr,
                "---",
                "",
                "# " <> title
              ]
          filePath =
            Slug.intoFilePathRelativeToDir "." slug.text
      writeFileUtf8 filePath content
      pure NoteInfo {day = Nothing, ..}

    writeFileUtf8 :: FilePath -> Text -> IO ()
    writeFileUtf8 path = BS.writeFile path . Text.encodeUtf8
