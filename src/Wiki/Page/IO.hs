-- | Utils for doing IO on pages
module Wiki.Page.IO where

import Data.Text.IO qualified as Text
import Language.LSP.Server
import Language.LSP.Types
import Language.LSP.Types.Lens as J hiding (to)
import Language.LSP.VFS
import MyPrelude
import System.Directory (getCurrentDirectory)
import Text.Pandoc.Definition (Pandoc)
import Wiki.Diagnostics
import Wiki.LSP.Config
import Wiki.LSP.Util
import Wiki.LinkTarget
import Wiki.Page.Formatting qualified as Formatting
import Wiki.Page.GotoDefinition qualified as GotoDefinition
import Wiki.Page.Parser qualified as Page
import Wiki.Page.Utils qualified as Page
import Wiki.Slug qualified as Slug

pageForSlug ::
  (MonadLsp Config m, MonadError ResponseError m) => Text -> m Pandoc
pageForSlug = do
  currentDirectory <- liftIO getCurrentDirectory
  let path = Slug.intoNormalizedUri currentDirectory slug
  m_vf <- getVirtualFile path
  contents <- case m_vf of
    Nothing ->
      rethrowIOException . liftIO . Text.readFile $
        Slug.intoFilePathRelativeToDir currentDirectory slug
    Just vf -> pure $ virtualFileText vf
  parseDocumentThrow contents
