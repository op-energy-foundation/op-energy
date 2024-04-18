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
import           Data.Default
import           Data.Proxy
import           Servant.API(ToHttpApiData(..), FromHttpApiData(..))

import           Data.OpEnergy.Account.API.V1.Account
import           Data.OpEnergy.Account.API.V1.BlockTimeStrike
import           Data.OpEnergy.Account.API.V1.UUID
import           Data.OpEnergy.Account.API.V1.Common
import           Data.OpEnergy.Account.API.V1.FilterRequest

share [mkPersist sqlSettings, mkMigrate "migrateBlockTimeStrikeGuess"] [persistLowerCase|
BlockTimeStrikeFutureGuess
  -- data
  guess SlowFast
  -- metadata
  creationTime POSIXTime
  -- reflinks
  person PersonId
  strike BlockTimeStrikeFutureId
  -- constraints
  UniquePersonStrikeGuess person strike -- only 1 guess per strike is allowed for person
  deriving Eq Show Generic

BlockTimeStrikePastGuess
  -- data
  guess SlowFast
  observedResult SlowFast
  -- metadata
  creationTime POSIXTime
  futureGuessCreationTime POSIXTime
  -- reflinks
  strike BlockTimeStrikePastId
  person PersonId
  -- constraints
  UniquePersonStrikeGuessResult person strike -- only 1 guess per strike is allowed for person
  deriving Eq Show Generic
|]

data BlockTimeStrikeGuessPublic = BlockTimeStrikeGuessPublic
  { person :: UUID Person
  , strike ::  BlockTimeStrikeFuture
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

data BlockTimeStrikeGuessPublicFilter = BlockTimeStrikeGuessPublicFilter
  { blockTimeStrikeGuessPublicFilterStrikeFutureCreationTimeGTE :: Maybe POSIXTime
  , blockTimeStrikeGuessPublicFilterStrikeFutureCreationTimeLTE   :: Maybe POSIXTime
  , blockTimeStrikeGuessPublicFilterCreationTimeGTE :: Maybe POSIXTime
  , blockTimeStrikeGuessPublicFilterCreationTimeLTE :: Maybe POSIXTime
  , blockTimeStrikeGuessPublicFilterGuessEQ :: Maybe SlowFast
  , blockTimeStrikeGuessPublicFilterGuessNEQ :: Maybe SlowFast
  , blockTimeStrikeGuessPublicFilterPersonEQ :: Maybe (UUID Person)
  , blockTimeStrikeGuessPublicFilterPersonNEQ :: Maybe (UUID Person)
  }
  deriving (Eq, Show, Generic)
instance Default BlockTimeStrikeGuessPublicFilter where
  def = defaultBlockTimeStrikeGuessPublicFilter
instance ToJSON BlockTimeStrikeGuessPublicFilter where
  toJSON v = genericToJSON (jsonCommonOptions v) v
  toEncoding v = genericToEncoding (jsonCommonOptions v) v
instance FromJSON BlockTimeStrikeGuessPublicFilter where
  parseJSON = genericParseJSON (jsonCommonOptions defaultBlockTimeStrikeGuessPublicFilter)
instance ToSchema BlockTimeStrikeGuessPublicFilter where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ (Text.unlines
      [ "This is the example of the public filter available for BlockTimeStrikeGuessPublic"
      , "each field is optional and can be provided in a combination of unique fields to build specific filter"
      ])
    & mapped.schema.example ?~ toJSON defaultBlockTimeStrikeGuessPublicFilter
instance ToParamSchema BlockTimeStrikeGuessPublicFilter where
  toParamSchema v = mempty
    & type_ ?~ SwaggerString
    & format ?~ (Text.decodeUtf8 $ BS.toStrict $ encode $ def1 v)
    where
      def1 :: Default a => Proxy a-> a
      def1 = def
instance ToHttpApiData BlockTimeStrikeGuessPublicFilter where
  toUrlPiece v = toUrlPiece $ Text.decodeUtf8 $ BS.toStrict $ encode v
  toQueryParam v = toQueryParam $ Text.decodeUtf8 $ BS.toStrict $ encode v
instance FromHttpApiData BlockTimeStrikeGuessPublicFilter where
  parseUrlPiece v = case Aeson.eitherDecodeStrict (Text.encodeUtf8 v) of
    Left some -> Left (Text.pack some)
    Right some -> Right some
  parseQueryParam v = case Aeson.eitherDecodeStrict (Text.encodeUtf8 v) of
    Left some -> Left (Text.pack some)
    Right some -> Right some
instance BuildFilter BlockTimeStrikeFutureGuess BlockTimeStrikeGuessPublicFilter where
  buildFilter v = List.concat
    [ maybe [] (\time-> [ BlockTimeStrikeFutureGuessCreationTime >=. time ])
      $ blockTimeStrikeGuessPublicFilterCreationTimeGTE v
    , maybe [] (\time-> [ BlockTimeStrikeFutureGuessCreationTime <=. time ])
      $  blockTimeStrikeGuessPublicFilterCreationTimeLTE v
    , maybe [] (\v-> [ BlockTimeStrikeFutureGuessGuess ==. v ])
      $  blockTimeStrikeGuessPublicFilterGuessEQ v
    , maybe [] (\v-> [ BlockTimeStrikeFutureGuessGuess !=. v ])
      $  blockTimeStrikeGuessPublicFilterGuessNEQ v
    ]
instance BuildFilter Person BlockTimeStrikeGuessPublicFilter where
  buildFilter v = List.concat
    [ maybe [] (\v-> [ PersonUuid ==. v ])
      $ blockTimeStrikeGuessPublicFilterPersonEQ v
    , maybe [] (\v-> [ PersonUuid !=. v ])
      $ blockTimeStrikeGuessPublicFilterPersonNEQ v
    ]

defaultBlockTimeStrikeGuessPublicFilter :: BlockTimeStrikeGuessPublicFilter
defaultBlockTimeStrikeGuessPublicFilter = BlockTimeStrikeGuessPublicFilter
  { blockTimeStrikeGuessPublicFilterStrikeFutureCreationTimeGTE = Just 1
  , blockTimeStrikeGuessPublicFilterStrikeFutureCreationTimeLTE = Just 1
  , blockTimeStrikeGuessPublicFilterCreationTimeGTE = Just 1
  , blockTimeStrikeGuessPublicFilterCreationTimeLTE = Just 1
  , blockTimeStrikeGuessPublicFilterGuessEQ = Just Slow
  , blockTimeStrikeGuessPublicFilterGuessNEQ = Just Fast
  , blockTimeStrikeGuessPublicFilterPersonEQ = Just defaultUUID
  , blockTimeStrikeGuessPublicFilterPersonNEQ = Just defaultUUID
  }


defaultBlockTimeStrikeGuessPublic :: BlockTimeStrikeGuessPublic
defaultBlockTimeStrikeGuessPublic = BlockTimeStrikeGuessPublic
  { person = defaultUUID
  , strike = defaultBlockTimeStrikeFuture
  , creationTime = defaultPOSIXTime
  , guess = defaultSlowFast
  }

data BlockTimeStrikeGuessResultPublic = BlockTimeStrikeGuessResultPublic
  { person :: UUID Person
  , strike :: BlockTimeStrikePast
  , creationTime :: POSIXTime
  , archiveTime :: POSIXTime
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
  , strike = defaultBlockTimeStrikePast
  , creationTime = defaultPOSIXTime
  , archiveTime = defaultPOSIXTime
  , guess = defaultSlowFast
  , observedResult = defaultSlowFast
  }

data BlockTimeStrikeGuessResultPublicFilter = BlockTimeStrikeGuessResultPublicFilter
  { blockTimeStrikeGuessResultPublicFilterPersonEQ                    :: Maybe (UUID Person)
  , blockTimeStrikeGuessResultPublicFilterPersonNEQ                   :: Maybe (UUID Person)
  , blockTimeStrikeGuessResultPublicFilterCreationTimeGTE             :: Maybe POSIXTime
  , blockTimeStrikeGuessResultPublicFilterCreationTimeLTE             :: Maybe POSIXTime
  , blockTimeStrikeGuessResultPublicFilterArchiveTimeGTE              :: Maybe POSIXTime
  , blockTimeStrikeGuessResultPublicFilterArchiveTimeLTE              :: Maybe POSIXTime
  , blockTimeStrikeGuessResultPublicFilterGuessEQ                     :: Maybe SlowFast
  , blockTimeStrikeGuessResultPublicFilterGuessNEQ                    :: Maybe SlowFast
  , blockTimeStrikeGuessResultPublicFilterObservedResultEQ            :: Maybe SlowFast
  , blockTimeStrikeGuessResultPublicFilterObservedResultNEQ           :: Maybe SlowFast
  }
  deriving (Eq, Show, Generic)
instance Default BlockTimeStrikeGuessResultPublicFilter where
  def = defaultBlockTimeStrikeGuessResultPublicFilter
instance ToJSON BlockTimeStrikeGuessResultPublicFilter where
  toJSON v = genericToJSON (jsonCommonOptions v) v
  toEncoding v = genericToEncoding (jsonCommonOptions v) v
instance FromJSON BlockTimeStrikeGuessResultPublicFilter where
  parseJSON = genericParseJSON (jsonCommonOptions defaultBlockTimeStrikeGuessResultPublicFilter)
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
    & format ?~ (Text.decodeUtf8 $ BS.toStrict $ encode $ def1 v)
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
instance BuildFilter BlockTimeStrikePastGuess BlockTimeStrikeGuessResultPublicFilter where
  buildFilter v = List.concat
    [ maybe [] (\v-> [BlockTimeStrikePastGuessFutureGuessCreationTime >=. v])
      $ blockTimeStrikeGuessResultPublicFilterCreationTimeGTE v
    , maybe [] (\v-> [BlockTimeStrikePastGuessFutureGuessCreationTime <=. v])
      $ blockTimeStrikeGuessResultPublicFilterCreationTimeLTE v
    , maybe [] (\v-> [BlockTimeStrikePastGuessCreationTime >=. v])
      $ blockTimeStrikeGuessResultPublicFilterArchiveTimeGTE v
    , maybe [] (\v-> [BlockTimeStrikePastGuessCreationTime <=. v])
      $ blockTimeStrikeGuessResultPublicFilterArchiveTimeLTE v
    , maybe [] (\v-> [BlockTimeStrikePastGuessGuess ==. v])
      $ blockTimeStrikeGuessResultPublicFilterGuessEQ v
    , maybe [] (\v-> [BlockTimeStrikePastGuessGuess !=. v])
      $ blockTimeStrikeGuessResultPublicFilterGuessNEQ v
    , maybe [] (\v-> [BlockTimeStrikePastGuessObservedResult ==. v])
      $ blockTimeStrikeGuessResultPublicFilterObservedResultEQ v
    , maybe [] (\v-> [BlockTimeStrikePastGuessObservedResult !=. v])
      $ blockTimeStrikeGuessResultPublicFilterObservedResultNEQ v
    ]
instance BuildFilter Person BlockTimeStrikeGuessResultPublicFilter where
  buildFilter v = List.concat
    [ maybe [] (\v-> [ PersonUuid ==. v ])
      $ blockTimeStrikeGuessResultPublicFilterPersonEQ v
    , maybe [] (\v-> [ PersonUuid !=. v ])
      $ blockTimeStrikeGuessResultPublicFilterPersonNEQ v
    ]

defaultBlockTimeStrikeGuessResultPublicFilter :: BlockTimeStrikeGuessResultPublicFilter
defaultBlockTimeStrikeGuessResultPublicFilter =  BlockTimeStrikeGuessResultPublicFilter
  { blockTimeStrikeGuessResultPublicFilterPersonEQ                    = Just defaultUUID
  , blockTimeStrikeGuessResultPublicFilterPersonNEQ                   = Just defaultUUID
  , blockTimeStrikeGuessResultPublicFilterCreationTimeGTE             = Just 1
  , blockTimeStrikeGuessResultPublicFilterCreationTimeLTE             = Just 1
  , blockTimeStrikeGuessResultPublicFilterArchiveTimeGTE              = Just 1
  , blockTimeStrikeGuessResultPublicFilterArchiveTimeLTE              = Just 1
  , blockTimeStrikeGuessResultPublicFilterGuessEQ                     = Just Slow
  , blockTimeStrikeGuessResultPublicFilterGuessNEQ                    = Just Fast
  , blockTimeStrikeGuessResultPublicFilterObservedResultEQ            = Just Slow
  , blockTimeStrikeGuessResultPublicFilterObservedResultNEQ           = Just Fast
  }
