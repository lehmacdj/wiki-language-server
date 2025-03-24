module Utils.FilePathUri where

import Language.LSP.Protocol.Types
import MyPrelude

nuriToFilePath :: NormalizedUri -> Maybe FilePath
nuriToFilePath = uriToFilePath . fromNormalizedUri

filepathToNuri :: FilePath -> NormalizedUri
filepathToNuri = toNormalizedUri . filePathToUri
