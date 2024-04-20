{-- | This module defines PagingResult data type.
 -- The purpose is to be used as a generic container of list values with paging
 --}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DeriveGeneric              #-}
module Data.OpEnergy.Account.API.V1.PagingResult where

import           Data.Swagger
import           Control.Lens
import           GHC.Generics
import           Data.Aeson as A
import           Data.Word
import           Data.List as List
import           Data.Char as Char
import           Data.Text as Text
import           Data.Default
import           Data.Proxy

-- | contains data for 1 page
data PagingResult a = PagingResult
  { pagingResultNextPage :: Maybe Word32
    -- ^ Nothing if there are no more pages coming
  , pagingResultCount:: Word32
    -- ^ records count. TOTAL, not count of elements of the page.
  , pagingResultResults :: [ a ]
    -- ^ contains list of records on a request page. Count of records defined by services' configs. For blocktime service, check configRecordsPerReply
  }
  deriving (Show, Generic)
instance FromJSON a => FromJSON (PagingResult a)
instance ToJSON a => ToJSON (PagingResult a) where
  toJSON = genericToJSON defaultOptions
    { A.fieldLabelModifier =
      (\s -> case s of
          [] -> []
          (h:t) -> (Char.toLower h):t
      ) . (List.drop (Text.length "PagingResult"))
    , A.constructorTagModifier = List.map Char.toLower
    }
  toEncoding = genericToEncoding defaultOptions
    { A.fieldLabelModifier =
      (\s -> case s of
          [] -> []
          (h:t) -> (Char.toLower h):t
      ) . (List.drop (Text.length "PagingResult"))
    , A.constructorTagModifier = List.map Char.toLower
    }
instance (Default a, ToJSON a, Show a) => ToSchema (PagingResult a) where
  declareNamedSchema v = return $ NamedSchema (Just ("PagingResult" <> Text.pack (List.takeWhile (/=' ') (show (List.head (pagingResultResults (def1 v))))))) $ mempty
    & type_ ?~ SwaggerObject
    & example ?~ toJSON (def1 v)
    where
      -- needed to satisfy type checker
      def1 :: Default b => Proxy b-> b
      def1 _ = def

instance Default a => Default (PagingResult a) where
  def = defaultPagingResult

-- | default PageResult
defaultPagingResult :: Default a => PagingResult a
defaultPagingResult = PagingResult
  { pagingResultNextPage = Nothing
  , pagingResultCount = 1
  , pagingResultResults = [ def ]
  }
