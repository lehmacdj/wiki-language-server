{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}

module Models.Page.Formatting
  ( editsForPage,
    FormattingOptions (..),
    FormattingOperation (..),
    textEditOfOperation,

    -- * tests
    spec_editsForPage,
    spec_textEditOfOperation,
  )
where

import Language.LSP.Protocol.Types
import Models.Page.TH
import Models.Page.Utils (attrRanges)
import Models.Slug (Slug (..))
import MyPrelude
import Text.Pandoc.Definition
import Text.Pandoc.Walk (query)

data FormattingOperation
  = -- | Represents an operation that converts a link like @[[random-id]]@ into
    -- a link like @<!--wls-->[[random-id|Note title]]@.
    WikilinkTransclusion
    { -- | The slug of the note the title of which should be transcluded
      slug :: Slug,
      -- | Range to replace with the link to the note
      linkReplaceRange :: Range,
      -- | Range to replace with a marker noting that this link is transcluded
      -- We use the marker to allow us to update wikilinks that we transcluded
      -- in the past. If there is a wikilink `[[slug|title]]` without the
      -- maker we don't transclude it to allow the user to edit the title
      -- manually when they want to.
      markerReplaceRange :: Range
    }
  | -- | Represents an operation that aligns a markdown table's columns
    -- by padding cells with spaces to make columns visually aligned.
    -- TODO: expand out the Table constructor's arguments to only include stuff
    TableAlignment
    { -- | Range of the table to replace
      tableRange :: Range,
      -- | The table AST to format
      table :: Block
    }
  deriving (Show, Eq, Ord)

pattern WlsTranscludedMarker :: Text
pattern WlsTranscludedMarker = "<!--wls-->"

-- | Marker indicating that a link was transcluded. Should occur directly after
-- a link in a span that indicates the range of the marker.
pattern WlsTranscluded :: Inline
pattern WlsTranscluded = RawInline (Format "html") WlsTranscludedMarker

pattern Positioned :: Range -> Inline -> Inline
pattern Positioned range x <- Span (attrRanges -> Just [range]) [x]

pattern PositionedLink :: Range -> [Inline] -> Text -> Inline
pattern PositionedLink range x slug <-
  Link (attrRanges -> Just [range]) x (slug, "wikilink")

editsForPage :: Pandoc -> [FormattingOperation]
editsForPage = query transcludeNoteTitles <> query alignTables
  where
    -- this traverses [Inline] instead of Inline so that we can do the slightly
    -- context dependent thing of looking for a RawInline that contains the
    -- magic comment.
    --
    -- N.B. There is a lot of pattern matching that would be very long here, so
    -- I'm using PatternSynonyms to keep it more readable.
    transcludeNoteTitles :: [Inline] -> [FormattingOperation]
    transcludeNoteTitles = \case
      -- if the link has a marker we rewrite it
      PositionedLink lr _ slug : Positioned mr WlsTranscluded : rest ->
        WikilinkTransclusion (Slug slug) lr mr : transcludeNoteTitles rest
      -- if the link has contents which are simply the slug we rewrite it
      PositionedLink lr@(Range _ markerPos) [Str contents] slug : rest
        | contents == slug ->
            -- the new marker will be put just after the link. This needs to be
            -- located 1 character past the last character that comprises the
            -- link because otherwise it would disrupt the link syntax
            let mr = Range markerPos markerPos
             in WikilinkTransclusion (Slug slug) lr mr : transcludeNoteTitles rest
      -- otherwise there is nothing to do
      [] -> []
      _ : rest -> transcludeNoteTitles rest

    alignTable :: Block -> [FormattingOperation]
    alignTable = \case
      Table attr caption colSpecs thead tbody tfoot ->
        case attrRanges attr of
          Just [range] -> [TableAlignment range (Table attr caption colSpecs thead tbody tfoot)]
          _ -> []
      _ -> []

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
        (Slug "asdf")
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
        (Slug "asdf")
        (Range start (Position 0 13))
        (Range (Position 0 13) (Position 0 13))
    )
    [md|[[asdf|asdf]]|]
  singleOperation
    "replaces link with marker unconditionally"
    ( WikilinkTransclusion
        (Slug "asdf")
        (Range start (Position 0 14))
        (Range (Position 0 14) (Position 0 24))
    )
    [md|[[asdf|Hello]]<!--wls-->|]
  singleOperation
    "also replaces link with marker without alternate title"
    ( WikilinkTransclusion
        (Slug "asdf")
        (Range start (Position 0 8))
        (Range (Position 0 8) (Position 0 18))
    )
    [md|[[asdf]]<!--wls-->|]
  noOperations
    "doesn't replace with modified text without marker"
    [md|[[asdf|Hello]]|]
  noOperations
    "doesn't replace with modified text with misplaced marker"
    [md|[[asdf|Hello]] <!--wls-->|]
  noOperations
    "doesn't replace with modified text with misplaced marker"
    [md|<!--wls-->[[asdf|Hello]]|]

  singleOperation
    "detects simple tables"
    (TableAlignment
      (Range (Position 0 0) (Position 2 24))
      (Table nullAttr (Caption Nothing []) [(AlignDefault, ColWidthDefault), (AlignDefault, ColWidthDefault)] (TableHead nullAttr [Row nullAttr [Cell nullAttr AlignDefault (RowSpan 1) (ColSpan 1) [Plain [Str "Header", Space, Str "1"]], 
                                             Cell nullAttr AlignDefault (RowSpan 1) (ColSpan 1) [Plain [Str "Header", Space, Str "2"]]]])
      [TableBody nullAttr (RowHeadColumns 0) [] [Row nullAttr [Cell nullAttr AlignDefault (RowSpan 1) (ColSpan 1) [Plain [Str "Cell", Space, Str "1"]], 
                                                                    Cell nullAttr AlignDefault (RowSpan 1) (ColSpan 1) [Plain [Str "Cell", Space, Str "2"]]]]]
      (TableFoot nullAttr [])))
    [md|
      | Header 1 | Header 2 |
      |-|-|
      | Cell 1 | Cell 2 |
    |]
  noOperations
    "doesn't detect non-table content as a table"
    [md|
      This is just regular text.
      No tables here.
    |]

textEditOfOperation ::
  (Monad m) =>
  -- | function that gets the title to use for the slug, if it returns Nothing
  -- no edit is produced for the formatting operation
  (Slug -> m (Maybe Text)) ->
  FormattingOperation ->
  m [TextEdit]
textEditOfOperation resolveSlugTitle = \case
  WikilinkTransclusion slug lr mr -> do
    m_title <- resolveSlugTitle slug
    fmap concat <$> for (toList m_title) $ \title ->
      pure
        [ TextEdit lr $ "[[" <> slug.text <> "|" <> title <> "]]",
          TextEdit mr WlsTranscludedMarker
        ]
  TableAlignment tableRange (Table _ _ colSpecs thead tbody tfoot) ->
    pure [TextEdit tableRange (formatTable colSpecs thead tbody tfoot)]
  TableAlignment _ _ -> pure [] -- This case shouldn't happen but let's be safe

spec_textEditOfOperation :: Spec
spec_textEditOfOperation = do
  let wikilinkTransclusion =
        WikilinkTransclusion
          (Slug "asdf")
          (Range (Position 0 0) (Position 0 14))
          (Range (Position 0 14) (Position 0 36))
  describe "WikilinkTransclusion" $ do
    it "returns a text edit when title is findable"
      $ textEditOfOperation
        (const (pure (Just "Some Title")))
        wikilinkTransclusion
      `shouldBe` Identity
        [ TextEdit (Range (Position 0 0) (Position 0 14)) "[[asdf|Some Title]]",
          TextEdit (Range (Position 0 14) (Position 0 36)) WlsTranscludedMarker
        ]
    it "doens't return anything when finding a title fails"
      $ textEditOfOperation
        (const (pure Nothing))
        wikilinkTransclusion
      `shouldBe` Identity []

  describe "TableAlignment" $ do
    it "formats a simple table correctly" $ do
      let tableBlock = Table 
            nullAttr 
            (Caption Nothing []) 
            [(AlignDefault, ColWidthDefault), (AlignDefault, ColWidthDefault)]
            (TableHead nullAttr [Row nullAttr [Cell nullAttr AlignDefault (RowSpan 1) (ColSpan 1) [Plain [Str "Header", Space, Str "1"]], 
                                               Cell nullAttr AlignDefault (RowSpan 1) (ColSpan 1) [Plain [Str "Header", Space, Str "2"]]]])
            [TableBody nullAttr (RowHeadColumns 0) [] [Row nullAttr [Cell nullAttr AlignDefault (RowSpan 1) (ColSpan 1) [Plain [Str "Cell", Space, Str "1"]], 
                                                                      Cell nullAttr AlignDefault (RowSpan 1) (ColSpan 1) [Plain [Str "Cell", Space, Str "2"]]]]]
            (TableFoot nullAttr [])
      let tableOp = TableAlignment (Range (Position 0 0) (Position 2 24)) tableBlock
      let result = textEditOfOperation (const (pure Nothing)) tableOp
      result `shouldBe` Identity 
        [ TextEdit (Range (Position 0 0) (Position 2 24)) 
            "| Header 1 | Header 2 |\n| -------- | -------- |\n| Cell 1   | Cell 2   |\n"
        ]