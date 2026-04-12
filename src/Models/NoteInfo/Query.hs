module Models.NoteInfo.Query where

import Data.IxSet.Typed qualified as IxSet
import Models.NoteInfo
import Models.Slug (Slug)
import MyPrelude

-- | Find all notes matching a specific day.
notesForDay :: Day -> NoteInfoCache -> [NoteInfo]
notesForDay day = IxSet.toList . (IxSet.@= Just day)

-- | Find a note by its slug.
noteForSlug :: Slug -> NoteInfoCache -> Maybe NoteInfo
noteForSlug slug cache =
  case IxSet.toList (cache IxSet.@= slug) of
    (note : _) -> Just note
    [] -> Nothing
