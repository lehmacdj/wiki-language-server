{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}

module Wiki.Page.Formatting
  ( editsForPage,
    FormattingOptions (..),
    FormattingOperation (..),

    -- * tests
    spec_editsForPage,
  )
where

import Language.LSP.Types
import MyPrelude
import TestPrelude
import Text.Pandoc.Definition
import Text.Pandoc.Walk (query)
import Wiki.Page (attrRanges)
import Wiki.Page.TH

data FormattingOperation
  = -- | Represents an operation that converts a link like @[[random-id]]@ into
    -- a link like @<!--wls-transcluded-->[[random-id|Note title]]@.
    WikilinkTransclusion
      { -- | The slug of the note the title of which should be transcluded
        slug :: Text,
        -- | Range to replace with the link to the note
        linkReplaceRange :: Range,
        -- | Range to replace with a marker explaining that this link is transcluded
        markerReplaceRange :: Range
      }
  | -- | see tweag/ormolu#882, should be able to remove this soonish (or
    -- obviously if I add another constructor.
    OrmoluWorkaroundConstructor Void
  deriving (Show, Eq, Ord)

-- | Marker indicating that a link was transcluded. Should occur directly after
-- a link in a span that indicates the range of the marker.
pattern WlsTranscluded :: Inline
pattern WlsTranscluded = RawInline (Format "html") "<!--wls-transcluded-->"

pattern Positioned :: Range -> Inline -> Inline
pattern Positioned range x <- Span (attrRanges -> Just [range]) [x]

pattern PositionedLink :: Range -> [Inline] -> Text -> Inline
pattern PositionedLink range x slug <-
  Link (attrRanges -> Just [range]) x (slug, "wikilink")

editsForPage :: Pandoc -> [FormattingOperation]
editsForPage = query transcludeNoteTitles
  where
    -- this traverses [Inline] instead of Inline so that we can do the slightly
    -- context dependent thing of looking for a RawInline that contains the
    -- magic comment.
    --
    -- N.B. There is a lot of pattern matching that would be very long here, so
    -- I'm using PatternSynonyms to keep it more readable.
    transcludeNoteTitles = \case
      -- if the link has a marker we rewrite it
      PositionedLink lr _ slug : Positioned mr WlsTranscluded : rest ->
        WikilinkTransclusion slug lr mr : transcludeNoteTitles rest
      -- if the link has contents which are simply the slug we rewrite it
      PositionedLink lr@(Range _ markerPos) [Str contents] slug : rest
        | contents == slug ->
          -- the new marker will be put just after the link. This needs to be
          -- located 1 character past the last character that comprises the
          -- link because otherwise it would disrupt the link syntax
          let mr = Range markerPos markerPos
           in WikilinkTransclusion slug lr mr : transcludeNoteTitles rest
      -- otherwise there is nothing to do
      [] -> []
      _ : rest -> transcludeNoteTitles rest

spec_editsForPage :: Spec
spec_editsForPage = do
  let singleOperation n fo p =
        it n $ editsForPage p `shouldBe` [fo]
  let noOperations n p =
        it n $ editsForPage p `shouldBe` []
  let start = Position 0 0
  singleOperation
    "replaces link without contents"
    ( WikilinkTransclusion
        "asdf"
        (Range start (Position 0 8))
        (Range (Position 0 8) (Position 0 8))
    )
    [md|[[asdf]]|]
  -- because we are just testing the contents and this is the same as what is
  -- represented in the AST by @[[asdf]]@ there isn't a great way to distinguish
  -- this. If I really felt like it was necessary we could probably use the
  -- source range to determine if the text is duplicated or not in the original
  -- source.
  singleOperation
    "explicit same title is same as no title (somewhat unfortunately)"
    ( WikilinkTransclusion
        "asdf"
        (Range start (Position 0 13))
        (Range (Position 0 13) (Position 0 13))
    )
    [md|[[asdf|asdf]]|]
  singleOperation
    "replaces link with marker unconditionally"
    ( WikilinkTransclusion
        "asdf"
        (Range start (Position 0 14))
        (Range (Position 0 14) (Position 0 36))
    )
    [md|[[asdf|Hello]]<!--wls-transcluded-->|]
  singleOperation
    "also replaces link with marker without alternate title"
    ( WikilinkTransclusion
        "asdf"
        (Range start (Position 0 8))
        (Range (Position 0 8) (Position 0 30))
    )
    [md|[[asdf]]<!--wls-transcluded-->|]
  noOperations
    "doesn't replace with modified text without marker"
    [md|[[asdf|Hello]]|]
  noOperations
    "doesn't replace with modified text with misplaced marker"
    [md|[[asdf|Hello]] <!--wls-transcluded-->|]
  noOperations
    "doesn't replace with modified text with misplaced marker"
    [md|<!--wls-transcluded-->[[asdf|Hello]]|]
