{-# LANGUAGE QuasiQuotes #-}

module TH.Utils where

import Data.Char (isSpace)
import Data.Text qualified as T
import MyPrelude
import TestPrelude

trimQuasiQuotation :: MonadFail m => String -> m Text
trimQuasiQuotation = trim . pack
  where
    nonEmptyFirst =
      fail "quasi quotation must either be one line or be empty on first line"
    nonEmptyLast =
      fail "quasi quotation must either be one line or only whitespace on last line"
    multiLine =
      fail "multiline quasi quote must have end marker on separate line and nothing on first line"
    sameSpacePrefixAsFirst =
      fail $
        "Each line must have at least as many spaces preceeding it as the first"
          <> " line or be empty.\n"
          <> "Preceding spaces will be stripped and empty lines will be treated"
          <> "as empty."
    notLastIsSpaces l = isJust (find (not . isSpace) l)
    emptyOrStripPrefix prefix t
      | T.null t = Just t
      | otherwise = stripPrefix prefix t
    trim x = case T.lines x of
      [] -> pure ""
      [l] -> pure l
      first : _ | first /= "" -> nonEmptyFirst
      first : (unsnoc -> Just (middle, last))
        | first /= "" -> nonEmptyFirst
        | notLastIsSpaces last -> nonEmptyLast
        | otherwise -> case middle of
          [] -> pure "\n"
          first : _ ->
            let prefix = T.takeWhile (== ' ') first
                stripped :: [Maybe Text]
                stripped = map (emptyOrStripPrefix prefix) middle
                verified = traverse (maybe sameSpacePrefixAsFirst pure) stripped
             in T.unlines <$> verified
      _ -> multiLine

spec_trimQuasiQuotation :: Spec
spec_trimQuasiQuotation = do
  it "works for single line" $
    trimQuasiQuotation [r|asdf|] `shouldBe` Just "asdf"
  it "works for empty multi line" $
    trimQuasiQuotation
      [r|
      |]
      `shouldBe` Just "\n"
  it "works for single line multi line" $
    trimQuasiQuotation
      [r|
        asdf
      |]
      `shouldBe` Just "asdf\n"
  it "works for multi line multi line" $
    trimQuasiQuotation
      [r|
        asdf
          foobar
      |]
      `shouldBe` Just "asdf\n  foobar\n"
  it "works with empty line as first line" $
    trimQuasiQuotation
      [r|

      asdf
      |]
      `shouldBe` Just "\n      asdf\n"
  it "works with empty line in middle of text" $
    trimQuasiQuotation
      [r|
      s

      asdf
      |]
      `shouldBe` Just "s\n\nasdf\n"
  it "works with empty line as last line" $
    trimQuasiQuotation
      [r|
      asdf

      |]
      `shouldBe` Just "asdf\n\n"
  it "fails with too little indentation on later lines" $
    trimQuasiQuotation
      [r|
        asdf
      foobar
      |]
      `shouldBe` Nothing
  it "fails with non-empty last line" $
    trimQuasiQuotation
      [r|
      asdf|]
      `shouldBe` Nothing
  it "fails with non-empty last line with other lines and correct indentation" $
    trimQuasiQuotation
      [r|
      asdf
      asdf|]
      `shouldBe` Nothing
  it "fails with non-empty first line for multi line" $
    trimQuasiQuotation
      [r|s
      asdf
      |]
      `shouldBe` Nothing
