module Executable.WikiNote (wikiNoteMain) where

import BackgroundTasks.UpdateNoteCache (noteCacheFileName)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as LBS
import Data.IxSet.Typed qualified as IxSet
import Data.Text.Encoding qualified as Text
import Effectful.FileSystem (runFileSystem)
import Executable.Options (LookupMode (..), OutputFormat (..))
import LSP.VFS (runVFSAccessPure)
import Models.NoteInfo
import Models.NoteInfo.CollectIO (collectNoteInfoForAllNotes)
import Models.NoteInfo.Serialization
  ( deserializeNoteInfoCache,
    serializeNoteInfoCache,
  )
import Models.Slug (Slug (..))
import Models.Slug qualified as Slug
import MyPrelude
import System.Directory (createDirectoryIfMissing, renameFile)
import System.Exit (exitFailure)
import System.FilePath (takeDirectory)
import System.IO (hPutStrLn)
import Utils.DateTimeParsing (NaturalLanguageParseError)
import Utils.DateTimeParsing qualified
import Utils.Diagnostics (runDiagnosticsNoOp)
import Utils.Logging (runLoggingNoOp)
import Utils.XdgPaths (statePath)

wikiNoteMain :: LookupMode -> OutputFormat -> Bool -> IO ()
wikiNoteMain lookupMode outputFormat createIfMissing = do
  cache <- loadCache
  case lookupMode of
    MatchingDate dateText -> handleDateLookup cache dateText
    MatchingTitle titleText -> handleTitleLookup cache titleText
    NewWithTitle titleText -> handleNewWithTitle cache titleText
  where
    handleDateLookup cache dateText = do
      now <- getZonedTime
      case parseDay now dateText of
        Left err -> do
          hPutStrLn stderr $ "Failed to parse date: " <> show err
          exitFailure
        Right day -> do
          let matches = IxSet.toList $ cache IxSet.@= Just day
          case matches of
            (note : _) -> outputNote note
            [] | createIfMissing -> do
              note <- createDateNote day
              saveCache (IxSet.insert note cache)
              outputNote note
            [] -> do
              cache' <- rescan
              let matches' =
                    IxSet.toList $ cache' IxSet.@= Just day
              case matches' of
                (note : _) -> outputNote note
                [] -> exitFailure

    handleTitleLookup cache titleText = do
      let matches = fuzzyMatchByTitle titleText cache
      case matches of
        (_ : _) -> outputNotes matches
        [] -> do
          cache' <- rescan
          let matches' = fuzzyMatchByTitle titleText cache'
          case matches' of
            (_ : _) -> outputNotes matches'
            [] | createIfMissing -> do
              note <- createTitleNote titleText
              saveCache (IxSet.insert note cache')
              outputNote note
            [] -> exitFailure

    handleNewWithTitle cache titleText = do
      -- Check cache first, then rescan to be sure
      case exactMatchByTitle titleText cache of
        Just note -> alreadyExistsError note
        Nothing -> do
          cache' <- rescan
          case exactMatchByTitle titleText cache' of
            Just note -> alreadyExistsError note
            Nothing -> do
              note <- createTitleNote titleText
              saveCache (IxSet.insert note cache')
              outputNote note
      where
        alreadyExistsError note = do
          let path =
                Slug.intoFilePathRelativeToDir "." note.slug.text
          hPutStrLn stderr $
            "Error: a note with this exact title "
              <> "already exists: "
              <> path
          exitFailure

    outputNote :: NoteInfo -> IO ()
    outputNote note = case outputFormat of
      OutputPath ->
        putStrLn $ pack $ Slug.intoFilePathRelativeToDir "." note.slug.text
      OutputSlug -> putStrLn note.slug.text

    outputNotes :: [NoteInfo] -> IO ()
    outputNotes = mapM_ outputNote

    parseDay :: ZonedTime -> Text -> Either NaturalLanguageParseError Day
    parseDay = Utils.DateTimeParsing.parseDay

    loadCache :: IO NoteInfoCache
    loadCache = do
      cachePath <- statePath noteCacheFileName
      result <- try @_ @SomeException $ BS.readFile cachePath
      case result of
        Left _ -> pure IxSet.empty
        Right bs -> case deserializeNoteInfoCache bs of
          Left _ -> pure IxSet.empty
          Right cache -> pure cache

    rescan :: IO NoteInfoCache
    rescan = do
      cache <-
        runEff
          . runFileSystem
          . runLoggingNoOp
          . runDiagnosticsNoOp
          . runVFSAccessPure
          $ collectNoteInfoForAllNotes
      saveCache cache
      pure cache

    saveCache :: NoteInfoCache -> IO ()
    saveCache cache = do
      cachePath <- statePath noteCacheFileName
      let contents =
            LBS.toStrict $
              Builder.toLazyByteString $
                serializeNoteInfoCache cache
      createDirectoryIfMissing True (takeDirectory cachePath)
      BS.writeFile (cachePath <> ".tmp") contents
      renameFile (cachePath <> ".tmp") cachePath
        `catch` \(_ :: SomeException) -> pure ()

    createDateNote :: Day -> IO NoteInfo
    createDateNote day = do
      slug <- Slug.generateRandomSlug
      now <- getZonedTime
      let title = pack $ formatTime defaultTimeLocale "%Y-%m-%d" day
          dateStr = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Ez" now
          content =
            unlines
              [ "---",
                "date: " <> pack dateStr,
                "---",
                "",
                "# " <> title
              ]
          filePath = Slug.intoFilePathRelativeToDir "." slug.text
      writeFileUtf8 filePath content
      pure NoteInfo {day = Just day, ..}

    createTitleNote :: Text -> IO NoteInfo
    createTitleNote title = do
      slug <- Slug.generateRandomSlug
      now <- getZonedTime
      let dateStr = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Ez" now
          content =
            unlines
              [ "---",
                "date: " <> pack dateStr,
                "---",
                "",
                "# " <> title
              ]
          filePath = Slug.intoFilePathRelativeToDir "." slug.text
      writeFileUtf8 filePath content
      pure NoteInfo {day = Nothing, ..}

    writeFileUtf8 :: FilePath -> Text -> IO ()
    writeFileUtf8 path = BS.writeFile path . Text.encodeUtf8
