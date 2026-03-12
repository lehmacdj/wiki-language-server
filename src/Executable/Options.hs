module Executable.Options
  ( Command (..),
    SkillsCommand (..),
    OutputFormat (..),
    LookupMode (..),
    opts,
  )
where

import MyPrelude
import Options.Applicative

data OutputFormat = OutputPath | OutputSlug
  deriving (Show, Eq)

data LookupMode
  = MatchingDate Text
  | MatchingTitle Text
  | NewWithTitle Text
  deriving (Show, Eq)

data SkillsCommand = SkillsSetup

data Command
  = LanguageServer
  | Note LookupMode OutputFormat Bool
  | Skills SkillsCommand

commandParser :: Parser Command
commandParser =
  subparser
    ( command
        "language-server"
        (info (pure LanguageServer) (progDesc "Start the language server"))
        <> command
          "note"
          (info noteParser (progDesc "Find or create a note"))
        <> command
          "skills"
          (info skillsParser (progDesc "Manage Claude Code skills"))
    )

skillsParser :: Parser Command
skillsParser =
  Skills
    <$> subparser
      ( command
          "setup"
          ( info
              (pure SkillsSetup)
              (progDesc "Install skills into .claude/skills/")
          )
      )

noteParser :: Parser Command
noteParser =
  Note
    <$> lookupModeParser
    <*> outputFormatParser
    <*> createIfMissingParser

lookupModeParser :: Parser LookupMode
lookupModeParser =
  MatchingDate
    <$> strOption
      ( long "matching-date"
          <> short 'd'
          <> metavar "DATE"
          <> help "Natural language date (e.g. \"today\", \"last thursday\")"
      )
      <|> MatchingTitle
    <$> strOption
      ( long "matching-title"
          <> short 't'
          <> metavar "TITLE"
          <> help "Fuzzy title match"
      )
      <|> NewWithTitle
    <$> strOption
      ( long "new-with-title"
          <> short 'n'
          <> metavar "TITLE"
          <> help
            "Create a new note with exact title \
            \(error if already exists)"
      )

outputFormatParser :: Parser OutputFormat
outputFormatParser =
  flag' OutputSlug (long "slug" <> help "Output slug instead of path")
    <|> flag' OutputPath (long "path" <> help "Output file path (default)")
    <|> pure OutputPath

opts :: ParserInfo Command
opts =
  info
    (commandParser <**> helper)
    ( fullDesc
        <> progDesc "Wiki utilities"
        <> header "wiki - a wiki language server and utilities"
    )

createIfMissingParser :: Parser Bool
createIfMissingParser =
  flag'
    False
    ( long "no-create-if-missing"
        <> help "Don't create a note if none matches"
    )
    <|> flag'
      True
      ( long "create-if-missing"
          <> help "Create a note if none matches (default)"
      )
    <|> pure True
