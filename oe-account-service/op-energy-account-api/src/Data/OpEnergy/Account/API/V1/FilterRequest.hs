{-- | This module defines FilterRequest data type.
 -- The purpose is to be used to build search filters
 --}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE AllowAmbiguousTypes        #-}
module Data.OpEnergy.Account.API.V1.FilterRequest
  ( BuildFilter(..)
  , FilterRequest(..)
  , defaultFilterRequest
  , mapFilter
  ) where

import           Control.Lens
import           Data.Swagger
import           Data.Aeson as A
import           Data.Aeson as Aeson
import           Data.Default
import           Data.Proxy
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import           Database.Persist
import           Database.Persist.Pagination
import           Servant.API(ToHttpApiData(..), FromHttpApiData(..))

class BuildFilter a b where
  buildFilter :: (b, Proxy a) -> [ Filter a]
  sortOrder   :: (b, Proxy a) -> SortOrder

newtype BuildFilter a b => FilterRequest a b = FilterRequest
  { unFilterRequest :: (b, Proxy a)
  }
  deriving (Eq, Show)

instance (FromJSON b, BuildFilter a b) => FromJSON (FilterRequest a b) where
  parseJSON v = FilterRequest <$> ((,) <$> parseJSON v <*> pure Proxy)
instance (BuildFilter a b, ToJSON b) => ToJSON (FilterRequest a b) where
  toJSON (FilterRequest (v, _)) = toJSON v
  toEncoding (FilterRequest (v, _)) = toEncoding v
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
  toUrlPiece (FilterRequest (v, _)) = toUrlPiece v
  toQueryParam (FilterRequest (v, _)) = toQueryParam v
instance (BuildFilter a b, FromJSON b) => FromHttpApiData (FilterRequest a b) where
  parseUrlPiece v = case Aeson.eitherDecodeStrict (Text.encodeUtf8 v) of
    Left some -> Left (Text.pack some)
    Right some -> Right (FilterRequest (some, Proxy))
  parseQueryParam v = case Aeson.eitherDecodeStrict (Text.encodeUtf8 v) of
    Left some -> Left (Text.pack some)
    Right some -> Right (FilterRequest (some, Proxy))


instance (Default b, BuildFilter a b) => Default (FilterRequest a b) where
  def = FilterRequest (def, Proxy)

instance Enum SortOrder where
  fromEnum Ascend = 0
  fromEnum Descend = 1
  toEnum 0 = Ascend
  toEnum 1 = Descend
  toEnum _ = error "toEnum: wrong value for SortOrder"
instance ToJSON SortOrder where
  toJSON Ascend = toJSON ("ascend" :: Text)
  toJSON Descend = toJSON ("descend" :: Text)
instance FromJSON SortOrder where
  parseJSON = withText "SortOrder" $! pure . verifySortOrder
instance ToSchema SortOrder where
  declareNamedSchema _ = pure $ NamedSchema (Just "SortOrder") $ mempty
    & type_ ?~ SwaggerString
instance ToParamSchema SortOrder where
  toParamSchema _ = mempty
    & type_ ?~ SwaggerString
    & enum_ ?~ (map toJSON $ enumFrom Ascend)
instance ToHttpApiData SortOrder where
  toUrlPiece Ascend = "ascend"
  toUrlPiece Descend = "descend"
instance FromHttpApiData SortOrder where
  parseUrlPiece "ascend" = Prelude.Right Ascend
  parseUrlPiece "descend" = Prelude.Right Descend
  parseUrlPiece _ = Left "parseUrlPiece: wrong SortOrder value"

verifySortOrder :: Text-> SortOrder
verifySortOrder "ascend" = Ascend
verifySortOrder "descend" = Descend
verifySortOrder _ = error "verifySortOrder: wrong value"

-- | default PageResult
defaultFilterRequest :: (BuildFilter a b, Default b) => FilterRequest a b
defaultFilterRequest = FilterRequest (def, Proxy)

mapFilter :: (BuildFilter a b, BuildFilter c b) => FilterRequest a b -> FilterRequest c b
mapFilter (FilterRequest (v, _)) = FilterRequest (v, Proxy)

