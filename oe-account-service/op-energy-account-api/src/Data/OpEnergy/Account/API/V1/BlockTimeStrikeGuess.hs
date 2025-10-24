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
module Data.OpEnergy.Account.API.V1.BlockTimeStrikeGuess where

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
  , strike = defaultBlockTimeStrike
  , creationTime = defaultPOSIXTime
  , guess = defaultSlowFast
  }

data BlockTimeStrikeGuessResult = BlockTimeStrikeGuessResult
  { person :: UUID Person
  , strike :: BlockTimeStrike
  , creationTime :: POSIXTime
  , guess :: SlowFast
  }
  deriving (Show, Generic)
instance ToJSON BlockTimeStrikeGuessResult
instance ToSchema BlockTimeStrikeGuessResult where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.type_ ?~ SwaggerObject
    & mapped.schema.example ?~ toJSON defaultBlockTimeStrikeGuessResult
    & mapped.schema.required .~
      [ "person"
      , "strike"
      , "creationTime"
      , "guess"
      ]
instance Default BlockTimeStrikeGuessResult where
  def = defaultBlockTimeStrikeGuessResult
defaultBlockTimeStrikeGuessResult :: BlockTimeStrikeGuessResult
defaultBlockTimeStrikeGuessResult = BlockTimeStrikeGuessResult
  { person = defaultUUID
  , strike = defaultBlockTimeStrike
  , creationTime = defaultPOSIXTime
  , guess = defaultSlowFast
  }

data BlockTimeStrikeGuessResultFilter = BlockTimeStrikeGuessResultFilter
    -- person
  { blockTimeStrikeGuessResultFilterPersonEQ              :: Maybe (UUID Person)
  , blockTimeStrikeGuessResultFilterPersonNEQ             :: Maybe (UUID Person)
    -- guess
  , blockTimeStrikeGuessResultFilterGuessEQ               :: Maybe SlowFast
  , blockTimeStrikeGuessResultFilterGuessNEQ              :: Maybe SlowFast
    -- observedResult
  , blockTimeStrikeGuessResultFilterObservedResultEQ      :: Maybe SlowFast
  , blockTimeStrikeGuessResultFilterObservedResultNEQ     :: Maybe SlowFast
    -- strike block height
  , blockTimeStrikeGuessResultFilterStrikeBlockHeightGTE  :: Maybe BlockHeight
  , blockTimeStrikeGuessResultFilterStrikeBlockHeightLTE  :: Maybe BlockHeight
  , blockTimeStrikeGuessResultFilterStrikeBlockHeightEQ   :: Maybe BlockHeight
  , blockTimeStrikeGuessResultFilterStrikeBlockHeightNEQ  :: Maybe BlockHeight
    -- strike strikeMediantime
  , blockTimeStrikeGuessResultFilterStrikeMediantimeGTE   :: Maybe POSIXTime
  , blockTimeStrikeGuessResultFilterStrikeMediantimeLTE   :: Maybe POSIXTime
  , blockTimeStrikeGuessResultFilterStrikeMediantimeEQ    :: Maybe POSIXTime
  , blockTimeStrikeGuessResultFilterStrikeMediantimeNEQ   :: Maybe POSIXTime
    -- sort
  , blockTimeStrikeGuessResultFilterSort                  :: Maybe SortOrder
  , blockTimeStrikeGuessResultFilterClass                 :: Maybe BlockTimeStrikeFilterClass
  , blockTimeStrikeGuessResultFilterLinesPerPage          :: Maybe (Positive Int)
  }
  deriving (Eq, Show, Generic)
instance Default BlockTimeStrikeGuessResultFilter where
  def = defaultBlockTimeStrikeGuessResultFilter
instance ToJSON BlockTimeStrikeGuessResultFilter where
  toJSON = commonToJSON genericToJSON
  toEncoding = commonToJSON genericToEncoding
instance FromJSON BlockTimeStrikeGuessResultFilter where
  parseJSON = commonParseJSON
instance ToSchema BlockTimeStrikeGuessResultFilter where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ (Text.unlines
      [ "This is the example of the public filter available for BlockTimeStrikeGuessResult"
      , "each field is optional and can be provided in a combination of unique fields to build specific filter"
      ])
    & mapped.schema.example ?~ toJSON defaultBlockTimeStrikeGuessResultFilter
instance ToParamSchema BlockTimeStrikeGuessResultFilter where
  toParamSchema v = mempty
    & type_ ?~ SwaggerString
    & format ?~ ( Text.unlines
                 $ List.map (<>",")
                 $ Text.splitOn ","
                 $ Text.decodeUtf8 $ BS.toStrict $ encode $ def1 v)
    where
      def1 :: Default a => Proxy a-> a
      def1 = def
instance ToHttpApiData BlockTimeStrikeGuessResultFilter where
  toUrlPiece v = toUrlPiece $ Text.decodeUtf8 $ BS.toStrict $ encode v
  toQueryParam v = toQueryParam $ Text.decodeUtf8 $ BS.toStrict $ encode v
instance FromHttpApiData BlockTimeStrikeGuessResultFilter where
  parseUrlPiece v = case Aeson.eitherDecodeStrict (Text.encodeUtf8 v) of
    Left some -> Left (Text.pack some)
    Right some -> Right some
  parseQueryParam v = case Aeson.eitherDecodeStrict (Text.encodeUtf8 v) of
    Left some -> Left (Text.pack some)
    Right some -> Right some

defaultBlockTimeStrikeGuessResultFilter :: BlockTimeStrikeGuessResultFilter
defaultBlockTimeStrikeGuessResultFilter =  BlockTimeStrikeGuessResultFilter
  { blockTimeStrikeGuessResultFilterPersonEQ              = Just defaultUUID
  , blockTimeStrikeGuessResultFilterPersonNEQ             = Just defaultUUID
  , blockTimeStrikeGuessResultFilterGuessEQ               = Just Slow
  , blockTimeStrikeGuessResultFilterGuessNEQ              = Just Fast
  , blockTimeStrikeGuessResultFilterObservedResultEQ      = Just Slow
  , blockTimeStrikeGuessResultFilterObservedResultNEQ     = Just Fast
  , blockTimeStrikeGuessResultFilterStrikeBlockHeightGTE  = Just 1
  , blockTimeStrikeGuessResultFilterStrikeBlockHeightLTE  = Just 1
  , blockTimeStrikeGuessResultFilterStrikeBlockHeightEQ   = Just 1
  , blockTimeStrikeGuessResultFilterStrikeBlockHeightNEQ  = Just 1
  , blockTimeStrikeGuessResultFilterStrikeMediantimeGTE   = Just 1
  , blockTimeStrikeGuessResultFilterStrikeMediantimeLTE   = Just 1
  , blockTimeStrikeGuessResultFilterStrikeMediantimeEQ    = Just 1
  , blockTimeStrikeGuessResultFilterStrikeMediantimeNEQ   = Just 1
  , blockTimeStrikeGuessResultFilterSort                  = Just Descend
  , blockTimeStrikeGuessResultFilterClass                 = Just defaultBlockTimeStrikeFilterClass
  , blockTimeStrikeGuessResultFilterLinesPerPage          = Just 100
  }
