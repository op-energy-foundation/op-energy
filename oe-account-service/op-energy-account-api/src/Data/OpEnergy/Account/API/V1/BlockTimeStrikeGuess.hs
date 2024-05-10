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

share [mkPersist sqlSettings, mkMigrate "migrateBlockTimeStrikeGuess"] [persistLowerCase|
BlockTimeStrikeGuess
  -- data
  guess SlowFast
  observedResult SlowFast Maybe -- when Nothing - guess, when Just _ - result
  blockHeight BlockHeight -- this field is for optimization purpose: it will be used as a loop termitation metric as there will be N -> inf guesses and we need to check only M of them, where blockHeight <= currentTip each scheduler check. Effectively, this is a copy strike.blockHeight
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
  , observedResult :: Maybe SlowFast
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
  { blockTimeStrikeGuessPublicFilterStrikeCreationTimeGTE :: Maybe POSIXTime
  , blockTimeStrikeGuessPublicFilterStrikeCreationTimeLTE   :: Maybe POSIXTime
  , blockTimeStrikeGuessPublicFilterCreationTimeGTE :: Maybe POSIXTime
  , blockTimeStrikeGuessPublicFilterCreationTimeLTE :: Maybe POSIXTime
  , blockTimeStrikeGuessPublicFilterGuessEQ :: Maybe SlowFast
  , blockTimeStrikeGuessPublicFilterGuessNEQ :: Maybe SlowFast
  , blockTimeStrikeGuessPublicFilterPersonEQ :: Maybe (UUID Person)
  , blockTimeStrikeGuessPublicFilterPersonNEQ :: Maybe (UUID Person)
  , blockTimeStrikeGuessPublicFilterBlockHeightGTE :: Maybe BlockHeight
  , blockTimeStrikeGuessPublicFilterBlockHeightLTE :: Maybe BlockHeight
  , blockTimeStrikeGuessPublicFilterNlocktimeGTE             :: Maybe POSIXTime
  , blockTimeStrikeGuessPublicFilterNlocktimeLTE             :: Maybe POSIXTime
    -- observedResult
  , blockTimeStrikeGuessPublicFilterObservedResultEQ            :: Maybe SlowFast
  , blockTimeStrikeGuessPublicFilterObservedResultNEQ           :: Maybe SlowFast
  , blockTimeStrikeGuessPublicFilterSort                     :: Maybe SortOrder
  }
  deriving (Eq, Show, Generic)
instance Default BlockTimeStrikeGuessPublicFilter where
  def = defaultBlockTimeStrikeGuessPublicFilter
instance ToJSON BlockTimeStrikeGuessPublicFilter where
  toJSON = commonToJSON genericToJSON
  toEncoding = commonToJSON genericToEncoding
instance FromJSON BlockTimeStrikeGuessPublicFilter where
  parseJSON = commonParseJSON
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
    & format ?~ ( Text.unlines
                 $ List.map (<>",")
                 $ Text.splitOn ","
                 $ Text.decodeUtf8 $ BS.toStrict $ encode $ def1 v
                )
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
instance BuildFilter BlockTimeStrikeGuess BlockTimeStrikeGuessPublicFilter where
  sortOrder (filter, _) = maybe Descend id (blockTimeStrikeGuessPublicFilterSort filter)
  buildFilter (v, _) = List.concat
    [ maybe [] (\time-> [ BlockTimeStrikeGuessCreationTime >=. time ])
      $ blockTimeStrikeGuessPublicFilterCreationTimeGTE v
    , maybe [] (\time-> [ BlockTimeStrikeGuessCreationTime <=. time ])
      $  blockTimeStrikeGuessPublicFilterCreationTimeLTE v
    , maybe [] (\v-> [ BlockTimeStrikeGuessGuess ==. v ])
      $  blockTimeStrikeGuessPublicFilterGuessEQ v
    , maybe [] (\v-> [ BlockTimeStrikeGuessGuess !=. v ])
      $  blockTimeStrikeGuessPublicFilterGuessNEQ v
    , maybe [] (\v-> [BlockTimeStrikeGuessObservedResult ==. Just v])
      $ blockTimeStrikeGuessPublicFilterObservedResultEQ v
    , maybe [] (\v-> [BlockTimeStrikeGuessObservedResult !=. Just v])
      $ blockTimeStrikeGuessPublicFilterObservedResultNEQ v
    ]
instance BuildFilter Person BlockTimeStrikeGuessPublicFilter where
  sortOrder (filter, _) = maybe Descend id (blockTimeStrikeGuessPublicFilterSort filter)
  buildFilter (v, _) = List.concat
    [ maybe [] (\v-> [ PersonUuid ==. v ])
      $ blockTimeStrikeGuessPublicFilterPersonEQ v
    , maybe [] (\v-> [ PersonUuid !=. v ])
      $ blockTimeStrikeGuessPublicFilterPersonNEQ v
    ]
instance BuildFilter BlockTimeStrike BlockTimeStrikeGuessPublicFilter where
  sortOrder (filter, _) = maybe Descend id (blockTimeStrikeGuessPublicFilterSort filter)
  buildFilter (v, _) = List.concat
    [ maybe [] (\v-> [ BlockTimeStrikeBlock >=. v ])
      $  blockTimeStrikeGuessPublicFilterBlockHeightGTE v
    , maybe [] (\v-> [ BlockTimeStrikeBlock <=. v ])
      $  blockTimeStrikeGuessPublicFilterBlockHeightLTE v
    , maybe [] (\v-> [ BlockTimeStrikeNlocktime >=. v ])
      $  blockTimeStrikeGuessPublicFilterNlocktimeGTE v
    , maybe [] (\v-> [ BlockTimeStrikeNlocktime <=. v ])
      $  blockTimeStrikeGuessPublicFilterNlocktimeLTE v
    ]

defaultBlockTimeStrikeGuessPublicFilter :: BlockTimeStrikeGuessPublicFilter
defaultBlockTimeStrikeGuessPublicFilter = BlockTimeStrikeGuessPublicFilter
  { blockTimeStrikeGuessPublicFilterStrikeCreationTimeGTE = Just 1
  , blockTimeStrikeGuessPublicFilterStrikeCreationTimeLTE = Just 1
  , blockTimeStrikeGuessPublicFilterCreationTimeGTE = Just 1
  , blockTimeStrikeGuessPublicFilterCreationTimeLTE = Just 1
  , blockTimeStrikeGuessPublicFilterGuessEQ = Just Slow
  , blockTimeStrikeGuessPublicFilterGuessNEQ = Just Fast
  , blockTimeStrikeGuessPublicFilterPersonEQ = Just defaultUUID
  , blockTimeStrikeGuessPublicFilterPersonNEQ = Just defaultUUID
  , blockTimeStrikeGuessPublicFilterBlockHeightGTE = Just 1
  , blockTimeStrikeGuessPublicFilterBlockHeightLTE = Just 1
  , blockTimeStrikeGuessPublicFilterNlocktimeGTE = Just 1
  , blockTimeStrikeGuessPublicFilterNlocktimeLTE = Just 1
    -- observedResult
  , blockTimeStrikeGuessPublicFilterObservedResultEQ  = Just Slow
  , blockTimeStrikeGuessPublicFilterObservedResultNEQ = Just Fast
  , blockTimeStrikeGuessPublicFilterSort         = Just Descend
  }


defaultBlockTimeStrikeGuessPublic :: BlockTimeStrikeGuessPublic
defaultBlockTimeStrikeGuessPublic = BlockTimeStrikeGuessPublic
  { person = defaultUUID
  , strike = defaultBlockTimeStrike
  , creationTime = defaultPOSIXTime
  , guess = defaultSlowFast
  , observedResult = Just Slow
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
  { blockTimeStrikeGuessResultPublicFilterPersonEQ                    :: Maybe (UUID Person)
  , blockTimeStrikeGuessResultPublicFilterPersonNEQ                   :: Maybe (UUID Person)
    -- creationTime
  , blockTimeStrikeGuessResultPublicFilterCreationTimeGTE             :: Maybe POSIXTime
  , blockTimeStrikeGuessResultPublicFilterCreationTimeLTE             :: Maybe POSIXTime
    -- archive time
  , blockTimeStrikeGuessResultPublicFilterArchiveTimeGTE              :: Maybe POSIXTime
  , blockTimeStrikeGuessResultPublicFilterArchiveTimeLTE              :: Maybe POSIXTime
    -- guess
  , blockTimeStrikeGuessResultPublicFilterGuessEQ                     :: Maybe SlowFast
  , blockTimeStrikeGuessResultPublicFilterGuessNEQ                    :: Maybe SlowFast
    -- observedResult
  , blockTimeStrikeGuessResultPublicFilterObservedResultEQ            :: Maybe SlowFast
  , blockTimeStrikeGuessResultPublicFilterObservedResultNEQ           :: Maybe SlowFast
    -- strike block height
  , blockTimeStrikeGuessResultPublicFilterObservedBlockHeightGTE      :: Maybe BlockHeight
  , blockTimeStrikeGuessResultPublicFilterObservedBlockHeightLTE      :: Maybe BlockHeight
    -- strike nlocktime
  , blockTimeStrikeGuessResultPublicFilterObservedNlocktimeGTE        :: Maybe POSIXTime
  , blockTimeStrikeGuessResultPublicFilterObservedNlocktimeLTE        :: Maybe POSIXTime
  , blockTimeStrikeGuessResultPublicFilterSort                        :: Maybe SortOrder
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
  buildFilter (v, _) = List.concat
    [ maybe [] (\v-> [BlockTimeStrikeGuessCreationTime >=. v])
      $ blockTimeStrikeGuessResultPublicFilterCreationTimeGTE v
    , maybe [] (\v-> [BlockTimeStrikeGuessCreationTime <=. v])
      $ blockTimeStrikeGuessResultPublicFilterCreationTimeLTE v
    , maybe [] (\v-> [BlockTimeStrikeGuessCreationTime >=. v])
      $ blockTimeStrikeGuessResultPublicFilterArchiveTimeGTE v
    , maybe [] (\v-> [BlockTimeStrikeGuessCreationTime <=. v])
      $ blockTimeStrikeGuessResultPublicFilterArchiveTimeLTE v
    , maybe [] (\v-> [BlockTimeStrikeGuessGuess ==. v])
      $ blockTimeStrikeGuessResultPublicFilterGuessEQ v
    , maybe [] (\v-> [BlockTimeStrikeGuessGuess !=. v])
      $ blockTimeStrikeGuessResultPublicFilterGuessNEQ v
    , maybe [] (\v-> [BlockTimeStrikeGuessObservedResult ==. Just v])
      $ blockTimeStrikeGuessResultPublicFilterObservedResultEQ v
    , maybe [] (\v-> [BlockTimeStrikeGuessObservedResult !=. Just v])
      $ blockTimeStrikeGuessResultPublicFilterObservedResultNEQ v
    ]
instance BuildFilter Person BlockTimeStrikeGuessResultPublicFilter where
  sortOrder (filter, _) = maybe Descend id (blockTimeStrikeGuessResultPublicFilterSort filter)
  buildFilter (v, _) = List.concat
    [ maybe [] (\v-> [ PersonUuid ==. v ])
      $ blockTimeStrikeGuessResultPublicFilterPersonEQ v
    , maybe [] (\v-> [ PersonUuid !=. v ])
      $ blockTimeStrikeGuessResultPublicFilterPersonNEQ v
    ]
instance BuildFilter BlockTimeStrike BlockTimeStrikeGuessResultPublicFilter where
  sortOrder (filter, _) = maybe Descend id (blockTimeStrikeGuessResultPublicFilterSort filter)
  buildFilter (v, _) = List.concat
        -- strike block height
    [ maybe [] (\v -> [ BlockTimeStrikeBlock >=. v])
      $ blockTimeStrikeGuessResultPublicFilterObservedBlockHeightGTE v
    , maybe [] (\v -> [ BlockTimeStrikeBlock <=. v])
      $ blockTimeStrikeGuessResultPublicFilterObservedBlockHeightLTE v
        -- strike nlocktime
    , maybe [] (\v -> [ BlockTimeStrikeNlocktime >=. v])
      $ blockTimeStrikeGuessResultPublicFilterObservedNlocktimeGTE v
    , maybe [] (\v -> [ BlockTimeStrikeNlocktime <=. v])
      $ blockTimeStrikeGuessResultPublicFilterObservedNlocktimeLTE v
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
  , blockTimeStrikeGuessResultPublicFilterObservedBlockHeightGTE      = Just 1
  , blockTimeStrikeGuessResultPublicFilterObservedBlockHeightLTE      = Just 1
    -- strike nlocktime
  , blockTimeStrikeGuessResultPublicFilterObservedNlocktimeGTE        = Just 1
  , blockTimeStrikeGuessResultPublicFilterObservedNlocktimeLTE        = Just 1
  , blockTimeStrikeGuessResultPublicFilterSort                        = Just Descend
  }
