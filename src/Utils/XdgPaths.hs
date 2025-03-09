module Utils.XdgPaths where

import MyPrelude
import System.Environment.XDG.BaseDir

xdgAppName :: String
xdgAppName = "wiki-language-server"

cachePath :: FilePath -> IO FilePath
cachePath = getUserCacheFile xdgAppName
