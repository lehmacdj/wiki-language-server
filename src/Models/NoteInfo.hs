module Models.NoteInfo where

import Language.LSP.Protocol.Types
import MyPrelude

data NoteInfo = NoteInfo
  { slug :: Text,
    title :: Text
    -- uri :: NormalizedUri,
    -- lastUpdated :: UTCTime
  }
  deriving stock (Generic)
