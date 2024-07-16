{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE StandaloneDeriving         #-}
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

import           Database.Persist.TH
import           Database.Persist
import           Database.Persist.Sql
import           Database.Persist.Pagination
import           Data.Default
import           Data.Proxy
import           Servant.API(ToHttpApiData(..), FromHttpApiData(..))

import           Data.OpEnergy.Account.API.V1.Account
import           Data.OpEnergy.Account.API.V1.BlockTimeStrike
import           Data.OpEnergy.Account.API.V1.UUID
import           Data.OpEnergy.Account.API.V1.Common
import           Data.OpEnergy.Account.API.V1.FilterRequest
import           Data.OpEnergy.API.V1.Block(BlockHeight)
import           Data.OpEnergy.API.V1.Positive(Positive)
import           Data.OpEnergy.Account.API.V1.BlockTimeStrikeFilterClass

share [mkPersist sqlSettings, mkMigrate "migrateBlockTimeStrikeGuess"] [persistLowerCase|
BlockTimeStrikeGuess
  -- data
  guess SlowFast
  -- metadata
  creationTime POSIXTime
  -- reflinks
  strike BlockTimeStrikeId
  person PersonId
  -- constraints
  UniqueBlockTimeStrikeGuessPersonStrike person strike -- only 1 guess per strike is allowed for person
  deriving Eq Show Generic
|]

data BlockTimeStrikeGuessPublic = BlockTimeStrikeGuessPublic
  { person :: UUID Person
  , strike ::  BlockTimeStrike
  , creationTime :: POSIXTime
  , guess :: SlowFast
  }
  deriving (Eq, Show, Generic)
instance ToJSON BlockTimeStrikeGuessPublic
instance ToSchema BlockTimeStrikeGuessPublic where
  declareNamedSchema _ = return $ NamedSchema (Just "BlockTimeStrikeGuessPublic") $ mempty
    & type_ ?~ SwaggerObject
    & example ?~ toJSON defaultBlockTimeStrikeGuessPublic
instance Default BlockTimeStrikeGuessPublic where
  def = defaultBlockTimeStrikeGuessPublic

defaultBlockTimeStrikeGuessPublic :: BlockTimeStrikeGuessPublic
defaultBlockTimeStrikeGuessPublic = BlockTimeStrikeGuessPublic
  { person = defaultUUID
  , strike = defaultBlockTimeStrike
  , creationTime = defaultPOSIXTime
  , guess = defaultSlowFast
  }

data BlockTimeStrikeGuessResultPublic = BlockTimeStrikeGuessResultPublic
  { person :: UUID Person
  , strike :: BlockTimeStrike
  , creationTime :: POSIXTime
  , guess :: SlowFast
  , observedResult :: SlowFast
  }
  deriving (Eq, Show, Generic)
instance ToJSON BlockTimeStrikeGuessResultPublic
instance ToSchema BlockTimeStrikeGuessResultPublic where
  declareNamedSchema _ = return $ NamedSchema (Just "BlockTimeStrikeGuessResultPublic") $ mempty
    & type_ ?~ SwaggerObject
    & example ?~ toJSON defaultBlockTimeStrikeGuessResultPublic
instance Default BlockTimeStrikeGuessResultPublic where
  def = defaultBlockTimeStrikeGuessResultPublic
defaultBlockTimeStrikeGuessResultPublic :: BlockTimeStrikeGuessResultPublic
defaultBlockTimeStrikeGuessResultPublic = BlockTimeStrikeGuessResultPublic
  { person = defaultUUID
  , strike = defaultBlockTimeStrike
  , creationTime = defaultPOSIXTime
  , guess = defaultSlowFast
  , observedResult = defaultSlowFast
  }

data BlockTimeStrikeGuessResultPublicFilter = BlockTimeStrikeGuessResultPublicFilter
    -- person
  { blockTimeStrikeGuessResultPublicFilterPersonEQ              :: Maybe (UUID Person)
  , blockTimeStrikeGuessResultPublicFilterPersonNEQ             :: Maybe (UUID Person)
    -- guess
  , blockTimeStrikeGuessResultPublicFilterGuessEQ               :: Maybe SlowFast
  , blockTimeStrikeGuessResultPublicFilterGuessNEQ              :: Maybe SlowFast
    -- observedResult
  , blockTimeStrikeGuessResultPublicFilterObservedResultEQ      :: Maybe SlowFast
  , blockTimeStrikeGuessResultPublicFilterObservedResultNEQ     :: Maybe SlowFast
    -- strike block height
  , blockTimeStrikeGuessResultPublicFilterStrikeBlockHeightGTE  :: Maybe BlockHeight
  , blockTimeStrikeGuessResultPublicFilterStrikeBlockHeightLTE  :: Maybe BlockHeight
  , blockTimeStrikeGuessResultPublicFilterStrikeBlockHeightEQ   :: Maybe BlockHeight
  , blockTimeStrikeGuessResultPublicFilterStrikeBlockHeightNEQ  :: Maybe BlockHeight
    -- strike strikeMediantime
  , blockTimeStrikeGuessResultPublicFilterStrikeMediantimeGTE   :: Maybe POSIXTime
  , blockTimeStrikeGuessResultPublicFilterStrikeMediantimeLTE   :: Maybe POSIXTime
  , blockTimeStrikeGuessResultPublicFilterStrikeMediantimeEQ    :: Maybe POSIXTime
  , blockTimeStrikeGuessResultPublicFilterStrikeMediantimeNEQ   :: Maybe POSIXTime
    -- sort
  , blockTimeStrikeGuessResultPublicFilterSort                  :: Maybe SortOrder
  , blockTimeStrikeGuessResultPublicFilterClass                 :: Maybe BlockTimeStrikeFilterClass
  , blockTimeStrikeGuessResultPublicFilterLinesPerPage          :: Maybe (Positive Int)
  }
  deriving (Eq, Show, Generic)
instance Default BlockTimeStrikeGuessResultPublicFilter where
  def = defaultBlockTimeStrikeGuessResultPublicFilter
instance ToJSON BlockTimeStrikeGuessResultPublicFilter where
  toJSON = commonToJSON genericToJSON
  toEncoding = commonToJSON genericToEncoding
instance FromJSON BlockTimeStrikeGuessResultPublicFilter where
  parseJSON = commonParseJSON
instance ToSchema BlockTimeStrikeGuessResultPublicFilter where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ (Text.unlines
      [ "This is the example of the public filter available for BlockTimeStrikeGuessResultPublic"
      , "each field is optional and can be provided in a combination of unique fields to build specific filter"
      ])
    & mapped.schema.example ?~ toJSON defaultBlockTimeStrikeGuessResultPublicFilter
instance ToParamSchema BlockTimeStrikeGuessResultPublicFilter where
  toParamSchema v = mempty
    & type_ ?~ SwaggerString
    & format ?~ ( Text.unlines
                 $ List.map (<>",")
                 $ Text.splitOn ","
                 $ Text.decodeUtf8 $ BS.toStrict $ encode $ def1 v)
    where
      def1 :: Default a => Proxy a-> a
      def1 = def
instance ToHttpApiData BlockTimeStrikeGuessResultPublicFilter where
  toUrlPiece v = toUrlPiece $ Text.decodeUtf8 $ BS.toStrict $ encode v
  toQueryParam v = toQueryParam $ Text.decodeUtf8 $ BS.toStrict $ encode v
instance FromHttpApiData BlockTimeStrikeGuessResultPublicFilter where
  parseUrlPiece v = case Aeson.eitherDecodeStrict (Text.encodeUtf8 v) of
    Left some -> Left (Text.pack some)
    Right some -> Right some
  parseQueryParam v = case Aeson.eitherDecodeStrict (Text.encodeUtf8 v) of
    Left some -> Left (Text.pack some)
    Right some -> Right some
instance BuildFilter BlockTimeStrikeGuess BlockTimeStrikeGuessResultPublicFilter where
  sortOrder (filter, _) = maybe Descend id (blockTimeStrikeGuessResultPublicFilterSort filter)
  buildFilter ( BlockTimeStrikeGuessResultPublicFilter
                _
                _
                -- guess
                mGuessEQ
                mGuessNEQ
                -- observedResult
                _
                _
                -- strike block height
                _
                _
                _
                _
                -- strike strikeMediantime
                _
                _
                _
                _
                -- sort
                _
                _
                _ -- lines per page
              , _
              ) = List.concat
    [ maybe [] (\v-> [BlockTimeStrikeGuessGuess ==. v]) mGuessEQ
    , maybe [] (\v-> [BlockTimeStrikeGuessGuess !=. v]) mGuessNEQ
    ]
instance BuildFilter Person BlockTimeStrikeGuessResultPublicFilter where
  sortOrder (filter, _) = maybe Descend id (blockTimeStrikeGuessResultPublicFilterSort filter)
  buildFilter ( BlockTimeStrikeGuessResultPublicFilter
                mPersonEQ
                mPersonNEQ
                -- guess
                _
                _
                -- observedResult
                _
                _
                -- strike block height
                _
                _
                _
                _
                -- strike strikeMediantime
                _
                _
                _
                _
                -- sort
                _
                _
                _ -- lines per page
              , _
              ) = List.concat
    [ maybe [] (\v-> [ PersonUuid ==. v ]) mPersonEQ
    , maybe [] (\v-> [ PersonUuid !=. v ]) mPersonNEQ
    ]
instance BuildFilter BlockTimeStrike BlockTimeStrikeGuessResultPublicFilter where
  sortOrder (filter, _) = maybe Descend id (blockTimeStrikeGuessResultPublicFilterSort filter)
  buildFilter ( BlockTimeStrikeGuessResultPublicFilter
                -- person
                _
                _
                -- guess
                _
                _
                -- observedResult
                mObservedResultEQ
                mObservedResultNEQ
                -- strike block height
                mStrikeBlockHeightGTE
                mStrikeBlockHeightLTE
                mStrikeBlockHeightEQ
                mStrikeBlockHeightNEQ
                -- strike strikeMediantime
                mStrikeMediantimeGTE
                mStrikeMediantimeLTE
                mStrikeMediantimeEQ
                mStrikeMediantimeNEQ
                -- sort
                _
                _
                _ -- lines per page
              , _
              ) = List.concat
        -- strike block height
    [ maybe [] (\v -> [ BlockTimeStrikeBlock >=. v]) mStrikeBlockHeightGTE
    , maybe [] (\v -> [ BlockTimeStrikeBlock <=. v]) mStrikeBlockHeightLTE
    , maybe [] (\v -> [ BlockTimeStrikeBlock ==. v]) mStrikeBlockHeightEQ
    , maybe [] (\v -> [ BlockTimeStrikeBlock !=. v]) mStrikeBlockHeightNEQ
    , maybe [] (\v -> [ BlockTimeStrikeStrikeMediantime >=. v]) mStrikeMediantimeGTE
    , maybe [] (\v -> [ BlockTimeStrikeStrikeMediantime <=. v]) mStrikeMediantimeLTE
    , maybe [] (\v -> [ BlockTimeStrikeStrikeMediantime ==. v]) mStrikeMediantimeEQ
    , maybe [] (\v -> [ BlockTimeStrikeStrikeMediantime !=. v]) mStrikeMediantimeNEQ
        -- strike observed result
    , maybe [] (\v -> [ BlockTimeStrikeObservedResult ==. Just v]) mObservedResultEQ
    , maybe [] (\v -> [ BlockTimeStrikeObservedResult !=. Just v]) mObservedResultNEQ
    ]

defaultBlockTimeStrikeGuessResultPublicFilter :: BlockTimeStrikeGuessResultPublicFilter
defaultBlockTimeStrikeGuessResultPublicFilter =  BlockTimeStrikeGuessResultPublicFilter
  { blockTimeStrikeGuessResultPublicFilterPersonEQ              = Just defaultUUID
  , blockTimeStrikeGuessResultPublicFilterPersonNEQ             = Just defaultUUID
  , blockTimeStrikeGuessResultPublicFilterGuessEQ               = Just Slow
  , blockTimeStrikeGuessResultPublicFilterGuessNEQ              = Just Fast
  , blockTimeStrikeGuessResultPublicFilterObservedResultEQ      = Just Slow
  , blockTimeStrikeGuessResultPublicFilterObservedResultNEQ     = Just Fast
  , blockTimeStrikeGuessResultPublicFilterStrikeBlockHeightGTE  = Just 1
  , blockTimeStrikeGuessResultPublicFilterStrikeBlockHeightLTE  = Just 1
  , blockTimeStrikeGuessResultPublicFilterStrikeBlockHeightEQ   = Just 1
  , blockTimeStrikeGuessResultPublicFilterStrikeBlockHeightNEQ  = Just 1
  , blockTimeStrikeGuessResultPublicFilterStrikeMediantimeGTE   = Just 1
  , blockTimeStrikeGuessResultPublicFilterStrikeMediantimeLTE   = Just 1
  , blockTimeStrikeGuessResultPublicFilterStrikeMediantimeEQ    = Just 1
  , blockTimeStrikeGuessResultPublicFilterStrikeMediantimeNEQ   = Just 1
  , blockTimeStrikeGuessResultPublicFilterSort                  = Just Descend
  , blockTimeStrikeGuessResultPublicFilterClass                 = Just defaultBlockTimeStrikeFilterClass
  , blockTimeStrikeGuessResultPublicFilterLinesPerPage          = Just 100
  }
