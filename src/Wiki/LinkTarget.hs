module Wiki.LinkTarget where

import Language.LSP.Types (Uri (Uri))
import MyPrelude
import System.Directory

data LinkTarget
  = -- | A slug, to be used in creating a wikilink. Most often this will be
    -- interpreted as relative to some directory and suffixed with ".md", but in
    -- theory it is possible to have a more clever strategy of interpreting this
    Wikilink Text
  | -- | An absolute filepath on the current machines filesystem. Equivalent to
    -- prefixing @file://@ before the path and using Uri instead
    AbsolutePath FilePath
  | -- | Requires a scheme
    OtherUri Uri
  deriving (Show)

-- | Interpret wikilinks as being relative to
relativeToWorkingDirectory :: MonadIO m => LinkTarget -> m Uri
relativeToWorkingDirectory link =
  liftIO $
    relativeToDir <$> getCurrentDirectory <*> pure link

-- | Given an absolute path to a directory interpret wiki links as being
-- relative to that directory
relativeToDir :: FilePath -> LinkTarget -> Uri
relativeToDir dir = \case
  Wikilink slug -> Uri $ "file://" <> pack dir <> "/" <> slug <> ".md"
  AbsolutePath p -> Uri . pack $ "file://" <> p
  OtherUri uri -> uri
