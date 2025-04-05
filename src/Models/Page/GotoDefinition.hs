{-# LANGUAGE QuasiQuotes #-}

module Models.Page.GotoDefinition where

import Data.Data.Lens (template)
import Data.Sequence (Seq (Empty, (:<|)))
import Data.Sequence.Lens (seqOf)
import Language.LSP.Protocol.Types (Position (Position), Uri (Uri), toNormalizedUri)
import Models.LinkTarget (LinkTarget (OtherUri, Wikilink))
import Models.Page.TH (md)
import Models.Page.Utils (attrB, attrI, attrRanges)
import Models.Slug (Slug (..))
import MyPrelude
import Text.Pandoc.Definition (Block, Inline (Link), Pandoc (..))
import Utils.RangePosition (positionInRange)

data BI = B Block | I Inline
  deriving (Show, Eq, Ord)

getLinkTargetAtPosition :: Pandoc -> Position -> Maybe LinkTarget
getLinkTargetAtPosition (Pandoc _meta blocks) p = go . fromList $ map B blocks
  where
    go Empty = Nothing
    go (B b@(preview (attrB . to attrRanges . _Just) -> Just ranges) :<| rest)
      | any (p `positionInRange`) ranges =
          go (seqOf (template . to B <> template . to I) b <> rest)
      | otherwise = go rest
    go (B b :<| rest) = go (seqOf (template . to B <> template . to I) b <> rest)
    go (I i@(preview (attrI . to attrRanges . _Just) -> Just ranges) :<| rest)
      | any (p `positionInRange`) ranges = case i of
          Link _ _ (slug, "wikilink") ->
            -- interpret wiki links as relative to the current file as determined
            -- by the position
            Just $ Wikilink $ Slug slug
          Link _ _ (url, _) ->
            -- TODO: ensure that url is a proper Uri in LSP sense, and possibly
            -- do some doctoring to make it so or otherwise don't return it
            -- as a link target
            Just . OtherUri . toNormalizedUri . Uri $ url
          _ -> go (seqOf (template . to B <> template . to I) i <> rest)
      | otherwise = go rest
    go (I i :<| rest) = go (seqOf (template . to B <> template . to I) i <> rest)

spec_getLinkTargetAtPosition :: Spec
spec_getLinkTargetAtPosition = do
  let simpleMarkdown =
        [md|
          # Title
          [[test]]
          [wikipedia](https://en.wikipedia.org)
          [[test|pipe title]]
          [[ test |<!--autogenerated-->pipe title]]
        |]
  let wikilink p =
        it ("detects wiki link at " <> show p)
          $ getLinkTargetAtPosition simpleMarkdown p
          `shouldBe` Just (Wikilink (Slug "test"))
  wikilink $ Position 1 0
  wikilink $ Position 1 2
  wikilink $ Position 1 5
  wikilink $ Position 1 7
  wikilink $ Position 3 7
  wikilink $ Position 4 7
  let normalLink p =
        it ("detects normal link at " <> show p)
          $ getLinkTargetAtPosition simpleMarkdown p
          `shouldBe` Just (OtherUri . toNormalizedUri . Uri $ "https://en.wikipedia.org")
  normalLink $ Position 2 0
  normalLink $ Position 2 10
  normalLink $ Position 2 11
  normalLink $ Position 2 36
  let noLink p =
        it ("fails to find a link at " <> show p)
          $ getLinkTargetAtPosition simpleMarkdown p
          `shouldBe` Nothing
  noLink $ Position 0 0
  noLink $ Position 0 4
  noLink $ Position 0 37
  noLink $ Position 1 37
  noLink $ Position 5 14
