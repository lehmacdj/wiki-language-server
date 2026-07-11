{-# LANGUAGE QuasiQuotes #-}

module Models.Page.THSpec where

import Models.Page.Parser qualified as Page
import Models.Page.TH
import MyPrelude

spec_mdTemplate :: Spec
spec_mdTemplate = do
  it "interpolates unescaped markdown text" do
    let title = "An *important* title"
    [md|# $(title)|]
      `shouldBe` (Page.parse "<QuasiQuoter>" "# An *important* title" & fromRight (error "parse failed"))
  it "allows parentheses in expressions" do
    [md|# $(toUpper ("hello" <> " world"))|]
      `shouldBe` (Page.parse "<QuasiQuoter>" "# HELLO WORLD" & fromRight (error "parse failed"))
  it "escapes a literal splice opener with $$" do
    [md|Shell: $$(command)|]
      `shouldBe` (Page.parse "<QuasiQuoter>" "Shell: $(command)" & fromRight (error "parse failed"))
  it "can render the underlying template as text" do
    let title = "Title"
    [mdText|# $(title)|] `shouldBe` "# Title"
