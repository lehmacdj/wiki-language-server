module Models.NoteInfo where

import Data.IxSet.Typed
import Models.Slug
import MyPrelude

data NoteInfo = NoteInfo
  { slug :: Slug,
    title :: Text
    -- perhaps want to also keep track of these for efficiency purposes:
    -- uri :: NormalizedUri,
    -- lastUpdated :: UTCTime
  }
  deriving stock (Show, Eq, Ord, Generic)

newtype Title = Title Text
  deriving stock (Show, Eq, Ord, Generic)

instance Indexable [Slug, Title] NoteInfo where
  indices =
    ixList
      (ixFun (pure . (.slug)))
      (ixFun (pure . Title . (.title)))

type NoteInfoCache = IxSet [Slug, Title] NoteInfo

fakeNoteInfoCache :: NoteInfoCache
fakeNoteInfoCache =
  Data.IxSet.Typed.fromList
    [ NoteInfo (Slug "kWp7rk0suUXd") "Hello world",
      NoteInfo (Slug "7Fu2PSiqrvz4") "Test world",
      NoteInfo (Slug "JQiVd3GmGPpP") "Some string",
      NoteInfo (Slug "acZlsJzsFs2g") "A wild unordinary herald"
    ]
