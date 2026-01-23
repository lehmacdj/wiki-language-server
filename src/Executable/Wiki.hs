module Executable.Wiki (wikiMain) where

import Executable.Options (Command (..), opts)
import Executable.WikiLanguageServer (wikiLanguageServerMain)
import MyPrelude
import Options.Applicative (execParser)

wikiMain :: IO ()
wikiMain = do
  cmd <- execParser opts
  case cmd of
    LanguageServer -> wikiLanguageServerMain
