{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE DerivingStrategies         #-}
module Data.OpEnergy.Account.API.V1.BlockTimeStrikeGuess
  ( BlockTimeStrikeGuess(..)
  , BlockTimeStrikeGuessFilter(..)
  ) where

import           Data.Swagger
import           Control.Lens
import           GHC.Generics
import           Data.Aeson as Aeson
import           Data.Time.Clock.POSIX(POSIXTime)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.ByteString.Lazy as BS
import qualified Data.List as List

import           Database.Persist.Pagination
import           Data.Default
import           Data.Proxy
import           Servant.API(ToHttpApiData(..), FromHttpApiData(..))

import           Data.OpEnergy.Account.API.V1.Account
import           Data.OpEnergy.Account.API.V1.BlockTimeStrike
import           Data.OpEnergy.Account.API.V1.UUID
import           Data.OpEnergy.Account.API.V1.Common
import           Data.OpEnergy.API.V1.Block(BlockHeight)
import           Data.OpEnergy.API.V1.Positive(Positive)
import           Data.OpEnergy.Account.API.V1.BlockTimeStrikeFilterClass
import           Data.OpEnergy.Account.API.V1.SlowFast


-- | This is the datatype for representing strike's guess through API
data BlockTimeStrikeGuess = BlockTimeStrikeGuess
  { person :: UUID Person
  , strike ::  BlockTimeStrike
  , creationTime :: POSIXTime
  , guess :: SlowFast
  }
  deriving (Eq, Show, Generic)
instance ToJSON BlockTimeStrikeGuess
instance ToSchema BlockTimeStrikeGuess where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.type_ ?~ SwaggerObject
    & mapped.schema.example ?~ toJSON defaultBlockTimeStrikeGuess
    & mapped.schema.required .~
      [ "person"
      , "strike"
      , "creationTime"
      , "guess"
      ]
instance Default BlockTimeStrikeGuess where
  def = defaultBlockTimeStrikeGuess

defaultBlockTimeStrikeGuess :: BlockTimeStrikeGuess
defaultBlockTimeStrikeGuess = BlockTimeStrikeGuess
  { person = defaultUUID
  , strike = def
  , creationTime = defaultPOSIXTime
  , guess = def
  }

data BlockTimeStrikeGuessFilter = BlockTimeStrikeGuessFilter
    -- person
  { blockTimeStrikeGuessFilterPersonEQ              :: Maybe (UUID Person)
  , blockTimeStrikeGuessFilterPersonNEQ             :: Maybe (UUID Person)
    -- guess
  , blockTimeStrikeGuessFilterGuessEQ               :: Maybe SlowFast
  , blockTimeStrikeGuessFilterGuessNEQ              :: Maybe SlowFast
    -- observed
  , blockTimeStrikeGuessFilterObservedResultEQ      :: Maybe SlowFast
  , blockTimeStrikeGuessFilterObservedResultNEQ     :: Maybe SlowFast
    -- strike block height
  , blockTimeStrikeGuessFilterStrikeBlockHeightGTE  :: Maybe BlockHeight
  , blockTimeStrikeGuessFilterStrikeBlockHeightLTE  :: Maybe BlockHeight
  , blockTimeStrikeGuessFilterStrikeBlockHeightEQ   :: Maybe BlockHeight
  , blockTimeStrikeGuessFilterStrikeBlockHeightNEQ  :: Maybe BlockHeight
    -- strike strikeMediantime
  , blockTimeStrikeGuessFilterStrikeMediantimeGTE   :: Maybe POSIXTime
  , blockTimeStrikeGuessFilterStrikeMediantimeLTE   :: Maybe POSIXTime
  , blockTimeStrikeGuessFilterStrikeMediantimeEQ    :: Maybe POSIXTime
  , blockTimeStrikeGuessFilterStrikeMediantimeNEQ   :: Maybe POSIXTime
    -- sort
  , blockTimeStrikeGuessFilterSort                  :: Maybe SortOrder
  , blockTimeStrikeGuessFilterClass                 :: Maybe BlockTimeStrikeFilterClass
  , blockTimeStrikeGuessFilterLinesPerPage          :: Maybe (Positive Int)
  }
  deriving (Eq, Show, Generic)
instance Default BlockTimeStrikeGuessFilter where
  def = defaultBlockTimeStrikeGuessFilter
instance ToJSON BlockTimeStrikeGuessFilter where
  toJSON = commonToJSON genericToJSON
  toEncoding = commonToJSON genericToEncoding
instance FromJSON BlockTimeStrikeGuessFilter where
  parseJSON = commonParseJSON
instance ToSchema BlockTimeStrikeGuessFilter where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ (Text.unlines
      [ "This is the example of the public filter available for BlockTimeStrikeGuess"
      , "each field is optional and can be provided in a combination of unique fields to build specific filter"
      ])
    & mapped.schema.example ?~ toJSON defaultBlockTimeStrikeGuessFilter
instance ToParamSchema BlockTimeStrikeGuessFilter where
  toParamSchema v = mempty
    & type_ ?~ SwaggerString
    & format ?~ ( Text.unlines
                 $ List.map (<>",")
                 $ Text.splitOn ","
                 $ Text.decodeUtf8 $ BS.toStrict $ encode $ def1 v)
    where
      def1 :: Default a => Proxy a-> a
      def1 = def
instance ToHttpApiData BlockTimeStrikeGuessFilter where
  toUrlPiece v = toUrlPiece $ Text.decodeUtf8 $ BS.toStrict $ encode v
  toQueryParam v = toQueryParam $ Text.decodeUtf8 $ BS.toStrict $ encode v
instance FromHttpApiData BlockTimeStrikeGuessFilter where
  parseUrlPiece v = case Aeson.eitherDecodeStrict (Text.encodeUtf8 v) of
    Left some -> Left (Text.pack some)
    Right some -> Right some
  parseQueryParam v = case Aeson.eitherDecodeStrict (Text.encodeUtf8 v) of
    Left some -> Left (Text.pack some)
    Right some -> Right some

defaultBlockTimeStrikeGuessFilter :: BlockTimeStrikeGuessFilter
defaultBlockTimeStrikeGuessFilter =  BlockTimeStrikeGuessFilter
  { blockTimeStrikeGuessFilterPersonEQ              = Just defaultUUID
  , blockTimeStrikeGuessFilterPersonNEQ             = Just defaultUUID
  , blockTimeStrikeGuessFilterGuessEQ               = Just Slow
  , blockTimeStrikeGuessFilterGuessNEQ              = Just Fast
  , blockTimeStrikeGuessFilterObservedResultEQ      = Just Slow
  , blockTimeStrikeGuessFilterObservedResultNEQ     = Just Fast
  , blockTimeStrikeGuessFilterStrikeBlockHeightGTE  = Just 1
  , blockTimeStrikeGuessFilterStrikeBlockHeightLTE  = Just 1
  , blockTimeStrikeGuessFilterStrikeBlockHeightEQ   = Just 1
  , blockTimeStrikeGuessFilterStrikeBlockHeightNEQ  = Just 1
  , blockTimeStrikeGuessFilterStrikeMediantimeGTE   = Just 1
  , blockTimeStrikeGuessFilterStrikeMediantimeLTE   = Just 1
  , blockTimeStrikeGuessFilterStrikeMediantimeEQ    = Just 1
  , blockTimeStrikeGuessFilterStrikeMediantimeNEQ   = Just 1
  , blockTimeStrikeGuessFilterSort                  = Just Descend
  , blockTimeStrikeGuessFilterClass                 = Just defaultBlockTimeStrikeFilterClass
  , blockTimeStrikeGuessFilterLinesPerPage          = Just 100
  }
