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
module Data.OpEnergy.Account.API.V1.BlockTimeStrike where

import           Data.Swagger
import           Control.Lens
import           GHC.Generics
import           Data.Aeson as Aeson
import           Data.Text(Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Data.Time.Clock.POSIX(POSIXTime)
import qualified Data.List as List
import qualified Data.ByteString.Lazy as BS

import           Servant.API(ToHttpApiData(..), FromHttpApiData(..))
import           Database.Persist.TH
import           Database.Persist
import           Database.Persist.Sql
import           Data.Proxy
import           Data.Default

import           Data.OpEnergy.API.V1.Block(BlockHeight, defaultBlockHeight)
import           Data.OpEnergy.Account.API.V1.Account
import           Data.OpEnergy.API.V1.Block(BlockHash)
import qualified Data.OpEnergy.API.V1.Hash as BlockHash (defaultHash)
import           Data.OpEnergy.Account.API.V1.FilterRequest
import           Data.OpEnergy.Account.API.V1.Common

share [mkPersist sqlSettings, mkMigrate "migrateBlockTimeStrike"] [persistLowerCase|
BlockTimeStrikeFuture
  -- data
  block BlockHeight
  nlocktime POSIXTime
  -- metadata
  creationTime POSIXTime
  -- constraints
  UniqueBlockTimeStrikeFutureBlockNLockTime block nlocktime
  deriving Eq Show Generic

BlockTimeStrikePast
  -- data
  block BlockHeight
  nlocktime POSIXTime
  observedResult SlowFast
  observedBlockMediantime POSIXTime
  observedBlockHash BlockHash
  -- metadata
  creationTime POSIXTime
  futureStrikeCreationTime  POSIXTime
  -- constraints
  UniqueBlockTimeStrikePastBlockNLockTime block nlocktime -- for now it is forbidden to have multiple strikes of the same (block,nlocktime) values
  deriving Eq Show Generic

BlockTimeStrikeFutureObservedBlock -- FSM state 1
  futureStrike BlockTimeStrikeFutureId
  futureStrikeBlock BlockHeight -- we use those value in guard checks: without cached values we will have to perform more DB IO, which seems unnecessary
  futureStrikeNlocktime POSIXTime -- we use those value in guard checks: without cached values we will have to perform more DB IO, which seems unnecessary
  pastStrike BlockTimeStrikePastId
-- handlersCountStartedProcess (Natural Int) : TODO: in the future, if we will have more than 1 handler of the block time strikes
-- handlersCountFinishedProcess (Natural Int) -- when handlersCountStartedProcess == handlersCountFinishedProcess and both are not 0, this means, that future strike is ready to be removed
  UniqueBlockTimeStrikeFutureObservedBlockFutureStrike futureStrike
  UniqueBlockTimeStrikeFutureObservedBlockPastStrike pastStrike
  deriving Eq Show Generic

BlockTimeStrikeFutureReadyToRemove -- FSM state 2: record here means, that all guesses had been moved to results and now future strike can be safely remove leaving past strike only
  futureStrike BlockTimeStrikeFutureId
  pastStrike BlockTimeStrikePastId
  UniqueBlockTimeStrikeFutureReadyToRemoveFutureStrike futureStrike
  UniqueBlockTimeStrikeFutureReadyToRemovePastStrike pastStrike
  deriving Eq Show Generic
|]

data BlockTimeStrikeFutureFilter = BlockTimeStrikeFutureFilter
  { blockTimeStrikeFutureFilterCreationTimeGTE :: Maybe POSIXTime
  , blockTimeStrikeFutureFilterCreationTimeLTE   :: Maybe POSIXTime
  , blockTimeStrikeFutureFilterNlocktimeGTE   :: Maybe POSIXTime
  , blockTimeStrikeFutureFilterNlocktimeLTE     :: Maybe POSIXTime
  , blockTimeStrikeFutureFilterBlockHeightGTE  :: Maybe BlockHeight
  , blockTimeStrikeFutureFilterBlockHeightLTE    :: Maybe BlockHeight
  }
  deriving (Eq, Show, Generic)
instance BuildFilter BlockTimeStrikeFuture BlockTimeStrikeFutureFilter where
  buildFilter filter = List.concat
    $ [ maybe [] (\time -> [ BlockTimeStrikeFutureCreationTime >=. time ])
        $ blockTimeStrikeFutureFilterCreationTimeGTE filter
      , maybe [] (\time -> [ BlockTimeStrikeFutureCreationTime <=. time ])
        $ blockTimeStrikeFutureFilterCreationTimeLTE filter
        -- median time
      , maybe [] (\time -> [ BlockTimeStrikeFutureNlocktime >=. time ])
        $ blockTimeStrikeFutureFilterNlocktimeGTE filter
      , maybe [] (\time -> [ BlockTimeStrikeFutureNlocktime <=. time ])
        $ blockTimeStrikeFutureFilterNlocktimeLTE filter
        -- blockheight
      , maybe [] (\time -> [ BlockTimeStrikeFutureBlock >=. time ])
        $ blockTimeStrikeFutureFilterBlockHeightGTE filter
      , maybe [] (\time -> [ BlockTimeStrikeFutureBlock <=. time ])
        $ blockTimeStrikeFutureFilterBlockHeightLTE filter
      ]
instance ToSchema BlockTimeStrikeFutureFilter where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ (Text.unlines
      [ "This is the example of the public filter available for BlockTimeStrikeFuture"
      , "each "
      ])
    & mapped.schema.example ?~ toJSON defaultBlockTimeStrikeFuture
instance ToJSON BlockTimeStrikeFutureFilter where
  toJSON = commonToJSON genericToJSON
  toEncoding = commonToJSON genericToEncoding
instance FromJSON BlockTimeStrikeFutureFilter where
  parseJSON = commonParseJSON
instance Default BlockTimeStrikeFutureFilter where
  def = defaultBlockTimeStrikeFutureFilter
instance ToParamSchema BlockTimeStrikeFutureFilter where
  toParamSchema v = mempty
    & type_ ?~ SwaggerString
    & format ?~ (Text.decodeUtf8 $ BS.toStrict $ encode $ def1 v)
    where
      def1 :: Default a => Proxy a-> a
      def1 = def
instance ToHttpApiData BlockTimeStrikeFutureFilter where
  toUrlPiece v = toUrlPiece $ Text.decodeUtf8 $ BS.toStrict $ encode v
  toQueryParam v = toQueryParam $ Text.decodeUtf8 $ BS.toStrict $ encode v
instance FromHttpApiData BlockTimeStrikeFutureFilter where
  parseUrlPiece v = case Aeson.eitherDecodeStrict (Text.encodeUtf8 v) of
    Left some -> Left (Text.pack some)
    Right some -> Right some
  parseQueryParam v = case Aeson.eitherDecodeStrict (Text.encodeUtf8 v) of
    Left some -> Left (Text.pack some)
    Right some -> Right some


defaultBlockTimeStrikeFutureFilter :: BlockTimeStrikeFutureFilter
defaultBlockTimeStrikeFutureFilter = BlockTimeStrikeFutureFilter
  { blockTimeStrikeFutureFilterCreationTimeGTE = Just 1
  , blockTimeStrikeFutureFilterCreationTimeLTE = Just 1
  , blockTimeStrikeFutureFilterNlocktimeGTE    = Just 1
  , blockTimeStrikeFutureFilterNlocktimeLTE    = Just 1
  , blockTimeStrikeFutureFilterBlockHeightGTE  = Just 1
  , blockTimeStrikeFutureFilterBlockHeightLTE  = Just 1
  }

defaultBlockTimeStrikeFuture :: BlockTimeStrikeFuture
defaultBlockTimeStrikeFuture = BlockTimeStrikeFuture
  { blockTimeStrikeFutureBlock = defaultBlockHeight
  , blockTimeStrikeFutureNlocktime = defaultPOSIXTime
  , blockTimeStrikeFutureCreationTime = defaultPOSIXTime
  }
instance ToSchema BlockTimeStrikeFuture where
  declareNamedSchema _ = return $ NamedSchema (Just "BlockTimeStrikeFuture") $ mempty
    & type_ ?~ SwaggerObject
    & example ?~ toJSON defaultBlockTimeStrikeFuture
instance ToJSON BlockTimeStrikeFuture where
  toJSON = commonToJSON genericToJSON
  toEncoding = commonToJSON genericToEncoding
instance FromJSON BlockTimeStrikeFuture where
  parseJSON = commonParseJSON
instance Default BlockTimeStrikeFuture where
  def = defaultBlockTimeStrikeFuture

instance ToSchema BlockTimeStrikePast where
  declareNamedSchema _ = return $ NamedSchema (Just "BlockTimeStrikePast") $ mempty
    & type_ ?~ SwaggerObject
    & example ?~ toJSON defaultBlockTimeStrikePast
defaultBlockTimeStrikePast :: BlockTimeStrikePast
defaultBlockTimeStrikePast = BlockTimeStrikePast
  { blockTimeStrikePastBlock = defaultBlockHeight
  , blockTimeStrikePastNlocktime = defaultPOSIXTime
  , blockTimeStrikePastCreationTime = defaultPOSIXTime
  , blockTimeStrikePastFutureStrikeCreationTime = defaultPOSIXTime
  , blockTimeStrikePastObservedResult = defaultSlowFast
  , blockTimeStrikePastObservedBlockMediantime = defaultPOSIXTime
  , blockTimeStrikePastObservedBlockHash = BlockHash.defaultHash
  }
instance ToJSON BlockTimeStrikePast where
  toJSON = commonToJSON genericToJSON
  toEncoding = commonToJSON genericToEncoding
instance FromJSON BlockTimeStrikePast where
  parseJSON = commonParseJSON
instance Default BlockTimeStrikePast where
  def = defaultBlockTimeStrikePast

data SlowFast
  = Slow
  | Fast
  deriving (Eq, Enum, Show, Generic)

instance ToJSON SlowFast where
  toJSON Slow = toJSON ("slow" :: Text)
  toJSON Fast = toJSON ("fast" :: Text)
instance FromJSON SlowFast where
  parseJSON = withText "SlowFast" $! pure . verifySlowFast
instance PersistField SlowFast where
  toPersistValue Slow = toPersistValue ("slow"::Text)
  toPersistValue Fast = toPersistValue ("fast"::Text)
  fromPersistValue (PersistText "slow") = Prelude.Right $! Slow
  fromPersistValue (PersistText "fast") = Prelude.Right $! Fast
  fromPersistValue _ = Left $ "fromPersistValue SlowFastGuess, expected Text"
instance PersistFieldSql SlowFast where
  sqlType _ = SqlString

instance ToSchema SlowFast where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~
      ( Text.unlines
        [ ""
        ]
      )
    & mapped.schema.example ?~ toJSON (map toJSON $ enumFrom Slow)
instance ToParamSchema SlowFast where
  toParamSchema _ = mempty
    & type_ ?~ SwaggerString
    & enum_ ?~ (map toJSON $ enumFrom Slow)
instance ToHttpApiData SlowFast where
  toUrlPiece Slow = "slow"
  toUrlPiece Fast = "fast"
instance FromHttpApiData SlowFast where
  parseUrlPiece "slow" = Prelude.Right Slow
  parseUrlPiece "fast" = Prelude.Right Fast
  parseUrlPiece _ = Left "wrong SlowFast value"

defaultSlowFast :: SlowFast
defaultSlowFast = Slow

verifySlowFast :: Text-> SlowFast
verifySlowFast "slow" = Slow
verifySlowFast "fast" = Fast
verifySlowFast _ = error "verifySlowFast: wrong value"

data BlockTimeStrikePastFilter = BlockTimeStrikePastFilter
    -- creation time
  { blockTimeStrikePastFilterCreationTimeGTE             :: Maybe POSIXTime
  , blockTimeStrikePastFilterCreationTimeLTE             :: Maybe POSIXTime
    -- blocktime
  , blockTimeStrikePastFilterNlocktimeGTE                :: Maybe POSIXTime
  , blockTimeStrikePastFilterNlocktimeLTE                :: Maybe POSIXTime
    -- block height
  , blockTimeStrikePastFilterBlockHeightGTE              :: Maybe BlockHeight
  , blockTimeStrikePastFilterBlockHeightLTE              :: Maybe BlockHeight
    -- observed result
  , blockTimeStrikePastFilterObservedResultEQ            :: Maybe SlowFast
  , blockTimeStrikePastFilterObservedResultNEQ           :: Maybe SlowFast
    -- observed block mediantime
  , blockTimeStrikePastFilterObservedMediantimeGTE       :: Maybe POSIXTime
  , blockTimeStrikePastFilterObservedMediantimeLTE       :: Maybe POSIXTime
    -- block hash
  , blockTimeStrikePastFilterObservedBlockHashEQ         :: Maybe BlockHash
  , blockTimeStrikePastFilterObservedBlockHashNEQ        :: Maybe BlockHash
    -- future strike creation time
  , blockTimeStrikePastFilterFutureStrikeCreationTimeGTE :: Maybe POSIXTime
  , blockTimeStrikePastFilterFutureStrikeCreationTimeLTE :: Maybe POSIXTime
  }
  deriving (Eq, Show, Generic)
instance ToJSON BlockTimeStrikePastFilter where
  toJSON = commonToJSON genericToJSON
  toEncoding = commonToJSON genericToEncoding
instance FromJSON BlockTimeStrikePastFilter where
  parseJSON = commonParseJSON
instance Default BlockTimeStrikePastFilter where
  def = defaultBlockTimeStrikePastFilter
instance ToSchema BlockTimeStrikePastFilter where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ (Text.unlines
      [ "This is the example of the public filter available for BlockTimeStrikePast"
      ])
    & mapped.schema.example ?~ toJSON defaultBlockTimeStrikeFuture
instance ToParamSchema BlockTimeStrikePastFilter where
  toParamSchema v = mempty
    & type_ ?~ SwaggerString
    & format ?~ (Text.decodeUtf8 $ BS.toStrict $ encode $ def1 v)
    where
      def1 :: Default a => Proxy a-> a
      def1 = def
instance ToHttpApiData BlockTimeStrikePastFilter where
  toUrlPiece v = toUrlPiece $ Text.decodeUtf8 $ BS.toStrict $ encode v
  toQueryParam v = toQueryParam $ Text.decodeUtf8 $ BS.toStrict $ encode v
instance FromHttpApiData BlockTimeStrikePastFilter where
  parseUrlPiece v = case Aeson.eitherDecodeStrict (Text.encodeUtf8 v) of
    Left some -> Left (Text.pack some)
    Right some -> Right some
  parseQueryParam v = case Aeson.eitherDecodeStrict (Text.encodeUtf8 v) of
    Left some -> Left (Text.pack some)
    Right some -> Right some
instance BuildFilter BlockTimeStrikePast BlockTimeStrikePastFilter where
  buildFilter v = List.concat
    [ maybe [] (\v-> [BlockTimeStrikePastCreationTime >=. v])
      $ blockTimeStrikePastFilterCreationTimeGTE v
    , maybe [] (\v-> [BlockTimeStrikePastCreationTime <=. v])
      $ blockTimeStrikePastFilterCreationTimeLTE v
    , maybe [] (\v-> [BlockTimeStrikePastNlocktime >=. v])
      $ blockTimeStrikePastFilterNlocktimeGTE v
    , maybe [] (\v-> [BlockTimeStrikePastNlocktime <=. v])
      $ blockTimeStrikePastFilterNlocktimeLTE v
    , maybe [] (\v-> [BlockTimeStrikePastBlock >=. v])
      $ blockTimeStrikePastFilterBlockHeightGTE v
    , maybe [] (\v-> [BlockTimeStrikePastBlock <=. v])
      $ blockTimeStrikePastFilterBlockHeightLTE v
    , maybe [] (\v-> [BlockTimeStrikePastObservedResult ==. v])
      $ blockTimeStrikePastFilterObservedResultEQ v
    , maybe [] (\v-> [BlockTimeStrikePastObservedResult !=. v])
      $ blockTimeStrikePastFilterObservedResultNEQ v
    , maybe [] (\v-> [BlockTimeStrikePastObservedBlockMediantime >=. v])
      $ blockTimeStrikePastFilterObservedMediantimeGTE v
    , maybe [] (\v-> [BlockTimeStrikePastObservedBlockMediantime <=. v])
      $ blockTimeStrikePastFilterObservedMediantimeLTE v
    , maybe [] (\v-> [BlockTimeStrikePastObservedBlockHash ==. v])
      $ blockTimeStrikePastFilterObservedBlockHashEQ v
    , maybe [] (\v-> [BlockTimeStrikePastObservedBlockHash !=. v])
      $ blockTimeStrikePastFilterObservedBlockHashNEQ v
    , maybe [] (\v-> [BlockTimeStrikePastFutureStrikeCreationTime >=. v])
      $ blockTimeStrikePastFilterFutureStrikeCreationTimeGTE v
    , maybe [] (\v-> [BlockTimeStrikePastFutureStrikeCreationTime <=. v])
      $ blockTimeStrikePastFilterFutureStrikeCreationTimeLTE v
    ]

defaultBlockTimeStrikePastFilter :: BlockTimeStrikePastFilter
defaultBlockTimeStrikePastFilter = BlockTimeStrikePastFilter
    -- creation time
  { blockTimeStrikePastFilterCreationTimeGTE             = Just 1
  , blockTimeStrikePastFilterCreationTimeLTE             = Just 1
    -- blocktime
  , blockTimeStrikePastFilterNlocktimeGTE                = Just 1
  , blockTimeStrikePastFilterNlocktimeLTE                = Just 1
    -- block height
  , blockTimeStrikePastFilterBlockHeightGTE              = Just 1
  , blockTimeStrikePastFilterBlockHeightLTE              = Just 1
    -- observed result
  , blockTimeStrikePastFilterObservedResultEQ            = Just Slow
  , blockTimeStrikePastFilterObservedResultNEQ           = Just Fast
    -- observed block mediantime
  , blockTimeStrikePastFilterObservedMediantimeGTE       = Just 1
  , blockTimeStrikePastFilterObservedMediantimeLTE       = Just 1
    -- block hash
  , blockTimeStrikePastFilterObservedBlockHashEQ         = Just BlockHash.defaultHash
  , blockTimeStrikePastFilterObservedBlockHashNEQ        = Just BlockHash.defaultHash
    -- future strike creation time
  , blockTimeStrikePastFilterFutureStrikeCreationTimeGTE = Just 1
  , blockTimeStrikePastFilterFutureStrikeCreationTimeLTE = Just 1
  }

