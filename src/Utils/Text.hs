module Utils.Text where

import MyPrelude

truncateWithMarker :: Int -> Text -> Text -> Text
truncateWithMarker maxLength marker text
  | length text <= maxLength = text
  | otherwise = take (maxLength - length marker) text <> marker
