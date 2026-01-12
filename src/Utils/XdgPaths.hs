module Utils.XdgPaths
  ( xdgAppName,
    cachePath,
    statePath,
  )
where

import MyPrelude
import System.Directory (XdgDirectory (..), getXdgDirectory)
import System.Environment.XDG.BaseDir

xdgAppName :: String
xdgAppName = "wiki-language-server"

cachePath :: FilePath -> IO FilePath
cachePath = getUserCacheFile xdgAppName

statePath :: FilePath -> IO FilePath
statePath relativePath = do
  stateDir <- getXdgDirectory XdgState xdgAppName
  pure $ stateDir </> relativePath
