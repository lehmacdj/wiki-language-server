module Models.Slug where

import Language.LSP.Protocol.Types (NormalizedUri, filePathToUri, toNormalizedUri)
import MyPrelude
import System.FilePath

newtype Slug = Slug {text :: Text}
  deriving stock (Show, Eq, Ord, Generic)

intoFilePathRelativeToDir :: FilePath -> Text -> FilePath
intoFilePathRelativeToDir dir slug = dir <> "/" <> unpack slug <> ".md"

intoUri :: FilePath -> Slug -> NormalizedUri
intoUri dir (Slug slug) =
  toNormalizedUri $ filePathToUri $ intoFilePathRelativeToDir dir slug

fromMarkdownFilePath :: FilePath -> Maybe Slug
fromMarkdownFilePath fp
  | takeExtension fp == ".md" = Just $ Slug $ pack $ dropExtension $ takeFileName fp
  | otherwise = Nothing
