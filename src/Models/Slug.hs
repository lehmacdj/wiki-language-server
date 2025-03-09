module Models.Slug where

import Language.LSP.Protocol.Types (NormalizedUri, toNormalizedUri)
import MyPrelude
import Models.Diagnostics (Uri (Uri))

intoFilePathRelativeToDir :: FilePath -> Text -> FilePath
intoFilePathRelativeToDir dir slug = dir <> "/" <> unpack slug <> ".md"

intoUri :: FilePath -> Text -> Uri
intoUri dir slug = Uri $ "file://" <> pack (intoFilePathRelativeToDir dir slug)

intoNormalizedUri :: FilePath -> Text -> NormalizedUri
intoNormalizedUri dir slug = toNormalizedUri $ intoUri dir slug
