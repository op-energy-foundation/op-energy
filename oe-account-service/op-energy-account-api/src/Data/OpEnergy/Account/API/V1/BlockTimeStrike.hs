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
import           Database.Persist.Pagination
import           Data.Proxy
import           Data.Default

import           Data.OpEnergy.API.V1.Block(BlockHeight, defaultBlockHeight)
import           Data.OpEnergy.Account.API.V1.Account
import           Data.OpEnergy.API.V1.Block(BlockHash)
import           Data.OpEnergy.API.V1.Positive(Positive)
import qualified Data.OpEnergy.API.V1.Hash as BlockHash (defaultHash)
import           Data.OpEnergy.Account.API.V1.FilterRequest
import           Data.OpEnergy.Account.API.V1.Common
import           Data.OpEnergy.Account.API.V1.BlockTimeStrikeFilterClass

share [mkPersist sqlSettings, mkMigrate "migrateBlockTimeStrike"] [persistLowerCase|
BlockTimeStrike
  -- data
  block BlockHeight
  strikeMediantime POSIXTime
  observedResult SlowFast Maybe
  observedBlockMediantime POSIXTime Maybe
  observedBlockHash BlockHash Maybe
  -- metadata
  creationTime POSIXTime
  -- constraints
  UniqueBlockTimeStrikeBlockStrikeMediantime block strikeMediantime -- for now it is forbidden to have multiple strikes of the same (block,strikeMediantime) values
  deriving Eq Show Generic

|]

data BlockTimeStrikeFilter = BlockTimeStrikeFilter
  { blockTimeStrikeFilterStrikeMediantimeGTE        :: Maybe POSIXTime
  , blockTimeStrikeFilterStrikeMediantimeLTE        :: Maybe POSIXTime
  , blockTimeStrikeFilterStrikeMediantimeEQ         :: Maybe POSIXTime
  , blockTimeStrikeFilterStrikeMediantimeNEQ        :: Maybe POSIXTime
  , blockTimeStrikeFilterStrikeBlockHeightGTE       :: Maybe BlockHeight
  , blockTimeStrikeFilterStrikeBlockHeightLTE       :: Maybe BlockHeight
  , blockTimeStrikeFilterStrikeBlockHeightEQ        :: Maybe BlockHeight
  , blockTimeStrikeFilterStrikeBlockHeightNEQ       :: Maybe BlockHeight
  , blockTimeStrikeFilterObservedBlockHashEQ        :: Maybe BlockHash
  , blockTimeStrikeFilterObservedBlockHashNEQ       :: Maybe BlockHash
  , blockTimeStrikeFilterSort                       :: Maybe SortOrder
  , blockTimeStrikeFilterClass                      :: Maybe BlockTimeStrikeFilterClass
  , blockTimeStrikeFilterLinesPerPage               :: Maybe (Positive Int)
  }
  deriving (Eq, Show, Generic)
instance ToSchema BlockTimeStrikeFilter where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ (Text.unlines
      [ "This is the example of the public filter available for BlockTimeStrike"
      , "each "
      ])
    & mapped.schema.example ?~ toJSON defaultBlockTimeStrike
instance ToJSON BlockTimeStrikeFilter where
  toJSON = commonToJSON genericToJSON
  toEncoding = commonToJSON genericToEncoding
instance FromJSON BlockTimeStrikeFilter where
  parseJSON = commonParseJSON
instance Default BlockTimeStrikeFilter where
  def = defaultBlockTimeStrikeFilter

defaultBlockTimeStrikeFilter :: BlockTimeStrikeFilter
defaultBlockTimeStrikeFilter = BlockTimeStrikeFilter
  { blockTimeStrikeFilterStrikeMediantimeGTE   = Just 1
  , blockTimeStrikeFilterStrikeMediantimeLTE   = Just 1
  , blockTimeStrikeFilterStrikeMediantimeEQ    = Just 1
  , blockTimeStrikeFilterStrikeMediantimeNEQ   = Just 2
  , blockTimeStrikeFilterStrikeBlockHeightGTE  = Just 1
  , blockTimeStrikeFilterStrikeBlockHeightLTE  = Just 1
  , blockTimeStrikeFilterStrikeBlockHeightEQ   = Just 1
  , blockTimeStrikeFilterStrikeBlockHeightNEQ  = Just 1
  , blockTimeStrikeFilterObservedBlockHashEQ   = Just BlockHash.defaultHash
  , blockTimeStrikeFilterObservedBlockHashNEQ  = Just BlockHash.defaultHash
  , blockTimeStrikeFilterSort                  = Just Descend
  , blockTimeStrikeFilterClass                 = Just BlockTimeStrikeFilterClassGuessable
  , blockTimeStrikeFilterLinesPerPage          = Just 100
  }

defaultBlockTimeStrike :: BlockTimeStrike
defaultBlockTimeStrike = BlockTimeStrike
  { blockTimeStrikeBlock = defaultBlockHeight
  , blockTimeStrikeStrikeMediantime = defaultPOSIXTime
  , blockTimeStrikeCreationTime = defaultPOSIXTime
  , blockTimeStrikeObservedResult = Just Slow
  , blockTimeStrikeObservedBlockMediantime = Just 1
  , blockTimeStrikeObservedBlockHash = Just BlockHash.defaultHash
  }
instance ToSchema BlockTimeStrike where
  declareNamedSchema _ = return $ NamedSchema (Just "BlockTimeStrike") $ mempty
    & type_ ?~ SwaggerObject
    & example ?~ toJSON defaultBlockTimeStrike
instance ToJSON BlockTimeStrike where
  toJSON = commonToJSON genericToJSON
  toEncoding = commonToJSON genericToEncoding
instance FromJSON BlockTimeStrike where
  parseJSON = commonParseJSON
instance Default BlockTimeStrike where
  def = defaultBlockTimeStrike

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

instance ToParamSchema BlockTimeStrikeFilter where
  toParamSchema v = mempty
    & type_ ?~ SwaggerString
    & format ?~ ( Text.unlines
                 $ List.map (<>",")
                 $ Text.splitOn ","
                 $ Text.decodeUtf8
                 $ BS.toStrict
                 $ encode
                 $ def1 v
                )
    where
      def1 :: Default a => Proxy a-> a
      def1 = def
instance ToHttpApiData BlockTimeStrikeFilter where
  toUrlPiece v = toUrlPiece $ Text.decodeUtf8 $ BS.toStrict $ encode v
  toQueryParam v = toQueryParam $ Text.decodeUtf8 $ BS.toStrict $ encode v
instance FromHttpApiData BlockTimeStrikeFilter where
  parseUrlPiece v = case Aeson.eitherDecodeStrict (Text.encodeUtf8 v) of
    Left some -> Left (Text.pack some)
    Right some -> Right some
  parseQueryParam v = case Aeson.eitherDecodeStrict (Text.encodeUtf8 v) of
    Left some -> Left (Text.pack some)
    Right some -> Right some
instance BuildFilter BlockTimeStrike BlockTimeStrikeFilter where
  sortOrder (filter, _) = maybe Descend id (blockTimeStrikeFilterSort filter)
  buildFilter ( BlockTimeStrikeFilter
                mstrikeMediantimeGTE
                mstrikeMediantimeLTE
                mstrikeMediantimeEQ
                mstrikeMediantimeNEQ
                mstrikeBlockHeightGTE
                mstrikeBlockHeightLTE
                mstrikeBlockHeightEQ
                mstrikeBlockHeightNEQ
                mobservedBlockHashEQ
                mobservedBlockHashNEQ
                _ -- sort
                _ -- class
                _ -- linesPerPage
              , _
              ) = List.concat
    [ maybe [] (\v-> [BlockTimeStrikeStrikeMediantime >=. v]) mstrikeMediantimeGTE
    , maybe [] (\v-> [BlockTimeStrikeStrikeMediantime <=. v]) mstrikeMediantimeLTE
    , maybe [] (\v-> [BlockTimeStrikeStrikeMediantime ==. v]) mstrikeMediantimeEQ
    , maybe [] (\v-> [BlockTimeStrikeStrikeMediantime !=. v]) mstrikeMediantimeNEQ
    , maybe [] (\v-> [BlockTimeStrikeBlock >=. v]) mstrikeBlockHeightGTE
    , maybe [] (\v-> [BlockTimeStrikeBlock <=. v]) mstrikeBlockHeightLTE
    , maybe [] (\v-> [BlockTimeStrikeBlock ==. v]) mstrikeBlockHeightEQ
    , maybe [] (\v-> [BlockTimeStrikeBlock !=. v]) mstrikeBlockHeightNEQ
    , maybe [] (\v-> [BlockTimeStrikeObservedBlockHash ==. Just v]) mobservedBlockHashEQ
    , maybe [] (\v-> [BlockTimeStrikeObservedBlockHash !=. Just v]) mobservedBlockHashNEQ
    ]
