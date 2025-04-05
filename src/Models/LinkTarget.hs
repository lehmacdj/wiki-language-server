-- This module stores the 'LinkTarget' type and functions for interpreting it
-- into 'FilePath' based on environmental information.
module Models.LinkTarget where

import Effectful.FileSystem
import Language.LSP.Protocol.Types (NormalizedUri)
import Models.Slug (Slug)
import Models.Slug qualified as Slug
import MyPrelude
import Utils.FilePathUri

-- | TODO: consider adding anchors; then we could also detect anchor links &
-- add them to getLinkTargetAtPosition
data LinkTarget
  = -- | A slug, to be used in creating a wikilink. Most often this will be
    -- interpreted as relative to some directory and suffixed with ".md", but in
    -- theory it is possible to have a more clever strategy of interpreting this
    Wikilink Slug
  | -- | An absolute filepath on the current machines filesystem. Equivalent to
    -- prefixing @file://@ before the path and using Uri instead
    AbsolutePath FilePath
  | -- | Requires a scheme
    OtherUri NormalizedUri
  deriving (Show, Eq, Ord)

-- | Interpret wikilinks as being relative to the working directory
relativeToWorkingDirectory :: (FileSystem :> es) => LinkTarget -> Eff es NormalizedUri
relativeToWorkingDirectory link =
  relativeToDir
    <$> getCurrentDirectory
    <*> pure link

-- | Given an absolute path to a directory interpret wiki links as being
-- relative to that directory
relativeToDir :: FilePath -> LinkTarget -> NormalizedUri
relativeToDir dir = \case
  Wikilink slug -> Slug.intoUri dir slug
  AbsolutePath p -> filepathToNuri p
  OtherUri uri -> uri
