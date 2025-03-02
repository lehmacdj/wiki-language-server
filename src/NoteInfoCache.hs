module NoteInfoCache where

-- import Data.ByteString.Lazy qualified as BSL
-- import Data.Serialize qualified as Cereal
-- import MyPrelude
-- import XdgPaths

-- noteInfoCacheFile :: IO FilePath
-- noteInfoCacheFile = cachePath "note_info.db"

-- -- | Corresponds to an entry in the note_info table of a SQLite database we
-- -- store at 'noteInfoCacheFile'.
-- data NoteInfo = NoteInfo
--   { slug :: Text,
--     title :: Text,
--     fileName :: Text,
--     lastUpdated :: UTCTime
--   }
--   deriving stock (Generic)

-- instance ToRow NoteInfo where
--   toRow NoteInfo {..} =
--     toRow (slug, title, fileName, lastModified)

-- instance FromRow NoteInfo where
--   fromRow = NoteInfo <$> field <*> field <*> field <*> field

-- class MonadPageTitle m where
--   getTitle :: Text -> m (Maybe Text)
