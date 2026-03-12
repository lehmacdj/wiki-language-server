module Executable.WikiSkills (wikiSkillsMain) where

import Data.ByteString qualified as BS
import Executable.Options (SkillsCommand (..))
import Language.Haskell.TH (litE, stringL)
import Language.Haskell.TH.Syntax (runIO)
import MyPrelude
import Paths_wiki_language_server (getDataDir)
import System.Directory
  ( createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    getCurrentDirectory,
    listDirectory,
    pathIsSymbolicLink,
    removeFile,
  )
import System.IO (hPutStrLn)
import System.Posix.Files (createSymbolicLink)

-- | The absolute path to the source repo's skills/ directory,
-- captured at compile time via TH.
--
-- When building from source, we want `wiki skills setup` to
-- symlink into the source tree rather than copying. This way
-- edits to the installed skill files (e.g. by Claude Code's
-- /address-comments) flow back to the repo and can be committed.
-- If the source directory no longer exists (e.g. the binary was
-- distributed to another machine), we fall back to copying from
-- the cabal data-files.
sourceSkillsDir :: FilePath
sourceSkillsDir =
  $( do
       cwd <- runIO getCurrentDirectory
       litE (stringL (cwd </> "skills"))
   )

wikiSkillsMain :: SkillsCommand -> IO ()
wikiSkillsMain SkillsSetup = setupSkills

setupSkills :: IO ()
setupSkills = do
  -- Check if the compile-time source path still exists
  sourceExists <- doesDirectoryExist sourceSkillsDir
  dataDir <- getDataDir
  -- Prefer source dir for symlinking, fall back to data dir
  let (skillsDir, useSymlinks) =
        if sourceExists
          then (sourceSkillsDir, True)
          else (dataDir, False)
  skillDirs <- listSkillDirs skillsDir
  when (null skillDirs) $
    hPutStrLn stderr "No skills found"
  let targetBase = ".claude" </> "skills"
  createDirectoryIfMissing True targetBase
  for_ skillDirs $ \skillName -> do
    let srcFile = skillsDir </> skillName </> "SKILL.md"
        targetDir = targetBase </> skillName
        targetFile = targetDir </> "SKILL.md"
    createDirectoryIfMissing True targetDir
    srcExists <- doesFileExist srcFile
    when srcExists $
      if useSymlinks
        then installSymlink srcFile targetFile
        else installCopy srcFile targetFile

installSymlink :: FilePath -> FilePath -> IO ()
installSymlink src target = do
  removeIfExists target
  createSymbolicLink src target
  putStrLn $ "  " <> pack target <> " -> " <> pack src

installCopy :: FilePath -> FilePath -> IO ()
installCopy src target = do
  removeIfExists target
  BS.readFile src >>= BS.writeFile target
  putStrLn $ "  " <> pack target <> " (copied)"

removeIfExists :: FilePath -> IO ()
removeIfExists path = do
  isLink <-
    pathIsSymbolicLink path
      `catch` \(_ :: SomeException) -> pure False
  isFile <- doesFileExist path
  when (isLink || isFile) $ removeFile path

-- | List subdirectories that contain a SKILL.md
listSkillDirs :: FilePath -> IO [FilePath]
listSkillDirs dir = do
  exists <- doesDirectoryExist dir
  if not exists
    then pure []
    else do
      entries <- listDirectory dir
      filterM
        (\e -> doesFileExist (dir </> e </> "SKILL.md"))
        entries
