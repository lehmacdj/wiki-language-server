module Models.NoteInfo.Serialization
  ( -- * Serialization
    serializeNoteInfoCache,
    serializeNoteInfo,
    deserializeNoteInfoCache,

    -- * Tests
    spec_serializeDeserializeRoundtrip,
  )
where

import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as LBS
import Data.IxSet.Typed qualified as IxSet
import Data.Text (splitOn)
import Data.Text.Encoding (decodeUtf8')
import Data.Text.Encoding qualified as Text
import Models.NoteInfo
import Models.Page.Utils (dayNoteTitleToDay)
import Models.Slug (Slug (..))
import MyPrelude hiding (Builder)

-- | Serialize NoteInfoCache to a Builder (TSV format: slug<TAB>title per line)
-- Note: day field is not serialized as it's derived from title
serializeNoteInfoCache :: NoteInfoCache -> Builder.Builder
serializeNoteInfoCache cache =
  mconcat
    [ serializeNoteInfo ni <> Builder.char8 '\n'
    | ni <- IxSet.toList cache
    ]

serializeNoteInfo :: NoteInfo -> Builder.Builder
serializeNoteInfo NoteInfo {slug = Slug slugText, title} =
  Builder.byteString (Text.encodeUtf8 slugText)
    <> Builder.char8 '\t'
    <> Builder.byteString (Text.encodeUtf8 title)

-- | Deserialize NoteInfoCache from ByteString (TSV format)
-- Returns Left with error message on parse failure
-- Note: day field is recomputed from title using dayNoteTitleToDay
deserializeNoteInfoCache :: ByteString -> Either Text NoteInfoCache
deserializeNoteInfoCache bs =
  case decodeUtf8' bs of
    Left err -> Left $ "UTF-8 decode error: " <> tshow err
    Right text -> parseLines text

parseLines :: Text -> Either Text NoteInfoCache
parseLines text = do
  noteInfos <- traverse parseLine (filter (not . null) (lines text))
  pure $ IxSet.fromList noteInfos

parseLine :: Text -> Either Text NoteInfo
parseLine line =
  case splitOn "\t" line of
    [slugText, titleText] ->
      let day = dayNoteTitleToDay titleText
       in Right $ NoteInfo (Slug slugText) titleText day
    _ -> Left $ "Invalid line format (expected slug<TAB>title): " <> line

-- | Convert Builder to strict ByteString
builderToByteString :: Builder.Builder -> ByteString
builderToByteString = LBS.toStrict . Builder.toLazyByteString

spec_serializeDeserializeRoundtrip :: Spec
spec_serializeDeserializeRoundtrip = describe "NoteInfo serialization" do
  let roundtrip cache =
        deserializeNoteInfoCache (builderToByteString $ serializeNoteInfoCache cache)

  it "roundtrips empty cache" do
    let cache = IxSet.empty
    roundtrip cache `shouldBe` Right cache

  it "roundtrips single entry" do
    let cache = IxSet.fromList [NoteInfo (Slug "test-slug") "Test Title" Nothing]
    roundtrip cache `shouldBe` Right cache

  it "roundtrips fakeNoteInfoCache" do
    roundtrip fakeNoteInfoCache `shouldBe` Right fakeNoteInfoCache

  it "handles titles with special characters" do
    let cache =
          IxSet.fromList
            [NoteInfo (Slug "id") "Title with: colons & ampersands" Nothing]
    roundtrip cache `shouldBe` Right cache

  it "rejects malformed input (missing tab)" do
    deserializeNoteInfoCache "slug-without-title\n"
      `shouldSatisfy` has _Left

  it "produces grep-friendly TSV format" do
    let cache = IxSet.fromList [NoteInfo (Slug "abc") "XYZ" Nothing]
    builderToByteString (serializeNoteInfoCache cache)
      `shouldBe` "abc\tXYZ\n"

  it "roundtrips day notes" do
    let cache =
          IxSet.fromList
            [NoteInfo (Slug "day1") "2023-08-15" (Just (fromGregorian 2023 8 15))]
    roundtrip cache `shouldBe` Right cache
