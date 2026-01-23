module Executable.Options
  ( Command (..),
    opts,
  )
where

import MyPrelude
import Options.Applicative

data Command
  = LanguageServer

commandParser :: Parser Command
commandParser =
  subparser
    ( command
        "language-server"
        (info (pure LanguageServer) (progDesc "Start the language server"))
    )

opts :: ParserInfo Command
opts =
  info
    (commandParser <**> helper)
    ( fullDesc
        <> progDesc "Wiki utilities"
        <> header "wiki - a wiki language server and utilities"
    )
