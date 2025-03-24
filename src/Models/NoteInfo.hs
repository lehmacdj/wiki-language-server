module Models.NoteInfo where

import MyPrelude

data NoteInfo = NoteInfo
  { slug :: Text,
    title :: Text
    -- perhaps want to also keep track of these for efficiency purposes:
    -- uri :: NormalizedUri,
    -- lastUpdated :: UTCTime
  }
  deriving stock (Show, Eq, Ord, Generic)
