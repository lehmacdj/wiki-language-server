module Models.NoteInfo where

import Data.IxSet.Typed
import Data.IxSet.Typed qualified as IxSet
import Models.Slug
import MyPrelude
import Text.Fuzzy (Fuzzy (Fuzzy))
import Text.Fuzzy qualified as Fuzzy

data NoteInfo = NoteInfo
  { slug :: Slug,
    title :: Text,
    day :: Maybe Day -- derived from title via dayNoteTitleToDay
  }
  deriving stock (Show, Eq, Ord, Generic)

newtype Title = Title Text
  deriving stock (Show, Eq, Ord, Generic)

instance Indexable [Slug, Title, Maybe Day] NoteInfo where
  indices =
    ixList
      (ixFun (pure . (.slug)))
      (ixFun (pure . Title . (.title)))
      (ixFun (pure . (.day)))

type NoteInfoCache = IxSet [Slug, Title, Maybe Day] NoteInfo

fuzzyMatchByTitle :: Text -> NoteInfoCache -> [NoteInfo]
fuzzyMatchByTitle query cache =
  Fuzzy.filter query (IxSet.toList cache) "" "" (.title) False
    <&> \(Fuzzy noteInfo _ _) -> noteInfo

exactMatchByTitle :: Text -> NoteInfoCache -> Maybe NoteInfo
exactMatchByTitle query cache =
  case IxSet.toList (cache IxSet.@= Title query) of
    (note : _) -> Just note
    [] -> Nothing

fakeNoteInfoCache :: NoteInfoCache
fakeNoteInfoCache =
  Data.IxSet.Typed.fromList
    [ NoteInfo (Slug "kWp7rk0suUXd") "Hello world" Nothing,
      NoteInfo (Slug "7Fu2PSiqrvz4") "Test world" Nothing,
      NoteInfo (Slug "JQiVd3GmGPpP") "Some string" Nothing,
      -- this title was chosen to contain "world" when fuzzy matched
      NoteInfo (Slug "acZlsJzsFs2g") "A wild unordinary herald" Nothing,
      NoteInfo (Slug "day1") "2023-08-15" (Just (fromGregorian 2023 8 15)),
      NoteInfo (Slug "day2") "2023-08-16 - Meeting" (Just (fromGregorian 2023 8 16))
    ]
