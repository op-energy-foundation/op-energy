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
  -- metadata
  creationTime POSIXTime
  -- constraints
  UniqueBlockTimeStrikeBlockStrikeMediantime block strikeMediantime -- for now it is forbidden to have multiple strikes of the same (block,strikeMediantime) values
  deriving Eq Show Generic

BlockTimeStrikeObserved
  -- data
  -- those fields are being kept as a sanity check in case of block chain
  -- reorganization as a last prove of the outcome. Though, we use confirmation
  -- algorithm, which goal is to reduce a possibility of hitting this case
  blockMediantime POSIXTime -- mediantime of the observed block.
  blockHash BlockHash -- hash of the observed block.
  -- metadata
  creationTime POSIXTime
  -- reflinks
  strike BlockTimeStrikeId
  -- constraints
  UniqueBlocktimeStrikeObservationStrike strike -- unique per strike
  deriving Eq Show Generic

-- this table contains
BlockTimeStrikeResult
  -- data
  result SlowFast
  -- metadata
  creationTime POSIXTime
  -- reflinks
  strike BlockTimeStrikeId
  -- constraints
  UniqueBlocktimeStrikeResultStrike strike -- unique per strike
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
  , blockTimeStrikeFilterSort                       :: Maybe StrikeSortOrder
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
  , blockTimeStrikeFilterSort                  = Just StrikeSortOrderDescend
  , blockTimeStrikeFilterClass                 = Just BlockTimeStrikeFilterClassGuessable
  , blockTimeStrikeFilterLinesPerPage          = Just 100
  }

defaultBlockTimeStrike :: BlockTimeStrike
defaultBlockTimeStrike = BlockTimeStrike
  { blockTimeStrikeBlock = defaultBlockHeight
  , blockTimeStrikeStrikeMediantime = defaultPOSIXTime
  , blockTimeStrikeCreationTime = defaultPOSIXTime
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

data BlockTimeStrikeObservedPublic =  BlockTimeStrikeObservedPublic
  { blockTimeStrikeObservedPublicBlockHash :: Maybe BlockHash
  , blockTimeStrikeObservedPublicCreationTime :: POSIXTime
  , blockTimeStrikeObservedPublicResult :: SlowFast
  , blockTimeStrikeObservedPublicBlockMediantime :: Maybe POSIXTime
  }
  deriving (Eq, Show, Generic)
defaultBlockTimeStrikeObservedPublic :: BlockTimeStrikeObservedPublic
defaultBlockTimeStrikeObservedPublic =  BlockTimeStrikeObservedPublic
  { blockTimeStrikeObservedPublicBlockHash = Just BlockHash.defaultHash
  , blockTimeStrikeObservedPublicCreationTime = defaultPOSIXTime
  , blockTimeStrikeObservedPublicResult = Slow
  , blockTimeStrikeObservedPublicBlockMediantime = Just 1
  }
instance ToSchema BlockTimeStrikeObservedPublic where
  declareNamedSchema _ = return $ NamedSchema (Just "BlockTimeStrikeObservedPublic") $ mempty
    & type_ ?~ SwaggerObject
    & example ?~ toJSON defaultBlockTimeStrikeObservedPublic
instance ToJSON BlockTimeStrikeObservedPublic where
  toJSON = commonToJSON genericToJSON
  toEncoding = commonToJSON genericToEncoding
instance FromJSON BlockTimeStrikeObservedPublic where
  parseJSON = commonParseJSON
instance Default BlockTimeStrikeObservedPublic where
  def = defaultBlockTimeStrikeObservedPublic

data SlowFast
  = Slow
  | Fast
  deriving (Eq, Enum, Show, Bounded, Ord, Generic)

instance ToJSON SlowFast where
  toJSON Slow = toJSON ("slow" :: Text)
  toJSON Fast = toJSON ("fast" :: Text)
instance FromJSON SlowFast where
  parseJSON = withText "SlowFast" $! pure . verifySlowFast
instance PersistField SlowFast where
  toPersistValue Slow = toPersistValue False
  toPersistValue Fast = toPersistValue True
  fromPersistValue (PersistBool False) = Prelude.Right Slow
  fromPersistValue (PersistBool True) = Prelude.Right Fast
  fromPersistValue _ = Left $ "fromPersistValue SlowFastGuess, unsupported value"
instance PersistFieldSql SlowFast where
  sqlType _ = SqlBool

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
  sortOrder (filter, _) = maybe Descend sortOrderFromStrikeSortOrder (blockTimeStrikeFilterSort filter)
  buildFilter ( BlockTimeStrikeFilter
                mstrikeMediantimeGTE
                mstrikeMediantimeLTE
                mstrikeMediantimeEQ
                mstrikeMediantimeNEQ
                mstrikeBlockHeightGTE
                mstrikeBlockHeightLTE
                mstrikeBlockHeightEQ
                mstrikeBlockHeightNEQ
                _  -- mobservedBlockHashEQ
                _  -- mobservedBlockHashNEQ
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
    ]
instance BuildFilter BlockTimeStrikeObserved BlockTimeStrikeFilter where
  sortOrder (filter, _) = maybe Descend sortOrderFromStrikeSortOrder (blockTimeStrikeFilterSort filter)
  buildFilter ( BlockTimeStrikeFilter
                _
                _
                _
                _
                _
                _
                _
                _
                mobservedBlockHashEQ
                mobservedBlockHashNEQ
                _ -- sort
                _ -- class
                _ -- linesPerPage
              , _
              ) = List.concat
    [ maybe [] (\v-> [BlockTimeStrikeObservedBlockHash ==. v]) mobservedBlockHashEQ
    , maybe [] (\v-> [BlockTimeStrikeObservedBlockHash !=. v]) mobservedBlockHashNEQ
    ]

-- | we need to use separate sort order as we can sort strikes by guesses count
data StrikeSortOrder
  = StrikeSortOrderAscend
  | StrikeSortOrderDescend
  | StrikeSortOrderAscendGuessesCount
  | StrikeSortOrderDescendGuessesCount
  deriving (Eq, Generic, Bounded, Enum)

instance ToJSON StrikeSortOrder where
  toJSON = toJSON . Text.pack . show
instance FromJSON StrikeSortOrder where
  parseJSON = withText "SortOrder" $! pure . verifyStrikeSortOrder
instance ToSchema StrikeSortOrder where
  declareNamedSchema _ = pure $ NamedSchema (Just "SortOrder") $ mempty
    & type_ ?~ SwaggerString
instance ToParamSchema StrikeSortOrder where
  toParamSchema _ = mempty
    & type_ ?~ SwaggerString
    & enum_ ?~ (map toJSON $ enumFrom Ascend)
instance ToHttpApiData StrikeSortOrder where
  toUrlPiece = Text.pack . show
instance FromHttpApiData StrikeSortOrder where
  parseUrlPiece = verifyStrikeSortOrderEither

instance Show StrikeSortOrder where
  show StrikeSortOrderAscend = "ascend"
  show StrikeSortOrderAscendGuessesCount = "ascend_guesses_count"
  show StrikeSortOrderDescend = "descend"
  show StrikeSortOrderDescendGuessesCount = "descend_guesses_count"

sortOrderFromStrikeSortOrder :: StrikeSortOrder -> SortOrder
sortOrderFromStrikeSortOrder StrikeSortOrderAscend = Ascend
sortOrderFromStrikeSortOrder StrikeSortOrderAscendGuessesCount = Ascend
sortOrderFromStrikeSortOrder StrikeSortOrderDescend = Descend
sortOrderFromStrikeSortOrder StrikeSortOrderDescendGuessesCount = Descend

verifyStrikeSortOrderEither :: Text -> Either Text StrikeSortOrder
verifyStrikeSortOrderEither t =
  case found of
    [] -> Left "verifyStrikeSortOrderEither: wrong value"
    ((v, _):_) -> Right v
  where
    found = List.dropWhile (\(_, v)-> v /= t) $ List.map (\v -> (v, Text.pack (show v))) $ enumFrom minBound

verifyStrikeSortOrder :: Text-> StrikeSortOrder
verifyStrikeSortOrder v =
  case verifyStrikeSortOrderEither v of
    Right some -> some
    Left some -> error $ Text.unpack some
