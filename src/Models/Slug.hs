module Models.Slug where

import Language.LSP.Protocol.Types (NormalizedUri, filePathToUri, toNormalizedUri)
import MyPrelude
import System.FilePath
import System.Random (randomRIO)

newtype Slug = Slug {text :: Text}
  deriving stock (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via FastGenericEncoding Slug

intoFilePathRelativeToDir :: FilePath -> Text -> FilePath
intoFilePathRelativeToDir dir slug = dir <> "/" <> unpack slug <> ".md"

intoUri :: FilePath -> Slug -> NormalizedUri
intoUri dir (Slug slug) =
  toNormalizedUri $ filePathToUri $ intoFilePathRelativeToDir dir slug

fromMarkdownFilePath :: FilePath -> Maybe Slug
fromMarkdownFilePath fp
  | takeExtension fp == ".md" = Just $ Slug $ pack $ dropExtension $ takeFileName fp
  | otherwise = Nothing

generateRandomSlug :: IO Slug
generateRandomSlug = Slug . pack <$> replicateM 12 randomBase62Char
  where
    base62Chars :: Vector Char
    base62Chars = fromList $ ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9']
    randomBase62Char :: IO Char
    randomBase62Char = (base62Chars `indexEx`) <$> randomRIO (0, length base62Chars - 1)
