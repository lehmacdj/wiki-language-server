module Executable.Wiki (wikiMain) where

import Executable.Options (Command (..), opts)
import Executable.WikiLanguageServer (wikiLanguageServerMain)
import Executable.WikiNote (wikiNoteMain)
import Executable.WikiSkills (wikiSkillsMain)
import MyPrelude
import Options.Applicative (execParser)

wikiMain :: IO ()
wikiMain = do
  cmd <- execParser opts
  case cmd of
    LanguageServer -> wikiLanguageServerMain
    Note lookupMode outputFormat createIfMissing ->
      wikiNoteMain lookupMode outputFormat createIfMissing
    Skills skillsCmd -> wikiSkillsMain skillsCmd
