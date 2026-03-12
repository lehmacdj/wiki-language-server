module Utils.Text where

import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Char (isControl)
import MyPrelude

truncateWithMarker :: Int -> Text -> Text -> Text
truncateWithMarker maxLength marker text
  | length text <= maxLength = text
  | otherwise = take (maxLength - length marker) text <> marker

-- | Strip control characters from all strings in a JSON value.
-- Control characters can cause issues in LSP clients (e.g. Neovim
-- treats strings containing null bytes as Blobs).
sanitizeJsonStrings :: Aeson.Value -> Aeson.Value
sanitizeJsonStrings = \case
  Aeson.String t -> Aeson.String (filter (not . isControl) t)
  Aeson.Array a -> Aeson.Array (fmap sanitizeJsonStrings a)
  Aeson.Object o ->
    Aeson.Object (KeyMap.map sanitizeJsonStrings o)
  v -> v

-- | Sanitize all string values in a JSON-serializable type by
-- stripping control characters. Falls back to the original value
-- if the roundtrip fails.
sanitizeResponse :: (ToJSON a, FromJSON a) => a -> a
sanitizeResponse a =
  case fromJSON (sanitizeJsonStrings (toJSON a)) of
    Right a' -> a'
    Left _ -> a
