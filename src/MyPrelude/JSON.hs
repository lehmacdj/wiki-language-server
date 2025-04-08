{-# LANGUAGE UndecidableInstances #-}

module MyPrelude.JSON
  ( FastGenericEncoding (..),
    MyPrelude.JSON.fromJSON,
    module X,
  )
where

import ClassyPrelude
import Data.Aeson
import Data.Aeson as X (FromJSON, ToJSON, eitherDecode, eitherDecodeStrict, encode, toJSON, Value(..))
import GHC.Generics

newtype FastGenericEncoding a = GenericEncodingAeson
  {underlying :: a}
  deriving (Generic)

instance
  (Generic a, GToJSON' Encoding Zero (Rep a), GToJSON' Value Zero (Rep a)) =>
  ToJSON (FastGenericEncoding a)
  where
  toJSON = genericToJSON defaultOptions . (.underlying)
  toEncoding = genericToEncoding defaultOptions . (.underlying)

instance
  (Generic a, GFromJSON Zero (Rep a)) =>
  FromJSON (FastGenericEncoding a)
  where
  parseJSON = fmap GenericEncodingAeson . genericParseJSON defaultOptions

-- | Version of 'Data.Aeson.fromJSON' that returns Either instead of Result
fromJSON :: (FromJSON a) => Value -> Either Text a
fromJSON = intoEither . Data.Aeson.fromJSON
  where
    intoEither = \case
      Error e -> Left $ pack e
      Success a -> Right a
