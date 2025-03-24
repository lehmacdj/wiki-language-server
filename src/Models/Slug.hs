module Models.Slug where

import Language.LSP.Protocol.Types (NormalizedUri, filePathToUri, toNormalizedUri)
import MyPrelude
import System.FilePath

intoFilePathRelativeToDir :: FilePath -> Text -> FilePath
intoFilePathRelativeToDir dir slug = dir <> "/" <> unpack slug <> ".md"

intoUri :: FilePath -> Text -> NormalizedUri
intoUri dir slug =
  toNormalizedUri $ filePathToUri $ intoFilePathRelativeToDir dir slug

fromMarkdownFilePath :: FilePath -> Maybe Text
fromMarkdownFilePath fp
  | takeExtension fp == ".md" = Just $ pack $ dropExtension $ takeFileName fp
  | otherwise = Nothing
