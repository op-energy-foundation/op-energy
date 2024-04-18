{-- | This module defines FilterRequest data type.
 -- The purpose is to be used to build search filters
 --}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
module Data.OpEnergy.Account.API.V1.FilterRequest
  ( BuildFilter(..)
  , FilterRequest(..)
  , defaultFilterRequest
  ) where

import           Data.Swagger
import           Data.Aeson as A
import           Data.Aeson as Aeson
import           Data.Default
import           Data.Proxy
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import           Database.Persist
import           Servant.API(ToHttpApiData(..), FromHttpApiData(..))

class BuildFilter a b where
  buildFilter :: b -> [ Filter a]

newtype BuildFilter a b => FilterRequest a b = FilterRequest
  { unFilterRequest :: b
  }
  deriving (Eq, Show)

instance (FromJSON b, BuildFilter a b) => FromJSON (FilterRequest a b) where
  parseJSON v = FilterRequest <$> parseJSON v
instance (BuildFilter a b, ToJSON b) => ToJSON (FilterRequest a b) where
  toJSON (FilterRequest v) = toJSON v
  toEncoding (FilterRequest v) = toEncoding v
instance (ToSchema b) => ToSchema (FilterRequest a b) where
  declareNamedSchema v = declareNamedSchema (def1 v)
    where
      -- needed to satisfy type checker
      def1 :: Proxy (FilterRequest a b)-> Proxy b
      def1 _ = Proxy
instance (ToParamSchema b) => ToParamSchema (FilterRequest a b) where
  toParamSchema v = toParamSchema (def1 v)
    where
      -- needed to satisfy type checker
      def1 :: Proxy (FilterRequest a b)-> Proxy b
      def1 _ = Proxy
instance (BuildFilter a b, ToHttpApiData b, ToJSON b) => ToHttpApiData (FilterRequest a b) where
  toUrlPiece (FilterRequest v) = toUrlPiece v
  toQueryParam (FilterRequest v) = toQueryParam v
instance (BuildFilter a b, FromJSON b) => FromHttpApiData (FilterRequest a b) where
  parseUrlPiece v = case Aeson.eitherDecodeStrict (Text.encodeUtf8 v) of
    Left some -> Left (Text.pack some)
    Right some -> Right (FilterRequest some)
  parseQueryParam v = case Aeson.eitherDecodeStrict (Text.encodeUtf8 v) of
    Left some -> Left (Text.pack some)
    Right some -> Right (FilterRequest some)


instance (Default b, BuildFilter a b) => Default (FilterRequest a b) where
  def = FilterRequest def

-- | default PageResult
defaultFilterRequest :: (BuildFilter a b, Default b) => FilterRequest a b
defaultFilterRequest = FilterRequest def

