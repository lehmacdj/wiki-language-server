module Utils.Text where

import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import MyPrelude

truncateWithMarker :: Int -> Text -> Text -> Text
truncateWithMarker maxLength marker text
  | length text <= maxLength = text
  | otherwise = take (maxLength - length marker) text <> marker

-- | Strip null characters from all strings in a JSON value.
-- Neovim treats strings containing null bytes as Blobs.
sanitizeJsonStrings :: Aeson.Value -> Aeson.Value
sanitizeJsonStrings = \case
  Aeson.String t -> Aeson.String (filter (/= '\NUL') t)
  Aeson.Array a -> Aeson.Array (fmap sanitizeJsonStrings a)
  Aeson.Object o ->
    Aeson.Object (KeyMap.map sanitizeJsonStrings o)
  v -> v

-- | Sanitize all string values in a JSON-serializable type by
-- stripping null characters. Falls back to the original value
-- if the roundtrip fails.
sanitizeResponse :: (ToJSON a, FromJSON a) => a -> a
sanitizeResponse a =
  case fromJSON (sanitizeJsonStrings (toJSON a)) of
    Right a' -> a'
    Left _ -> a

spec_sanitizeJsonStrings :: Spec
spec_sanitizeJsonStrings = do
  it "strips null characters from nested JSON strings" $
    sanitizeJsonStrings
      ( Aeson.object
          [ "values"
              Aeson..= ["before\NULafter", "\NUL" :: Text]
          ]
      )
      `shouldBe` Aeson.object
        ["values" Aeson..= ["beforeafter", "" :: Text]]
  it "preserves Markdown line breaks and indentation" $
    sanitizeJsonStrings (Aeson.String markdown)
      `shouldBe` Aeson.String markdown
  where
    markdown = "# Title\n\n- item\n\tcontinued\r\n"
