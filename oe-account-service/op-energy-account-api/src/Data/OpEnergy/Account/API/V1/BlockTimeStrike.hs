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
import           Data.Word(Word64)

import           Servant.API(ToHttpApiData(..), FromHttpApiData(..))
import           Database.Persist.Pagination
import           Data.Proxy
import           Data.Default

import           Data.OpEnergy.API.V1.Block(BlockHash, BlockHeight, defaultBlockHeight)
import           Data.OpEnergy.Account.API.V1.Account
import           Data.OpEnergy.API.V1.Positive(Positive)
import qualified Data.OpEnergy.API.V1.Hash as BlockHash (defaultHash)
import           Data.OpEnergy.Account.API.V1.FilterRequest()
import           Data.OpEnergy.Account.API.V1.Common
import           Data.OpEnergy.Account.API.V1.BlockTimeStrikeFilterClass

data BlockTimeStrike = BlockTimeStrike
  { blockTimeStrikeBlock            :: BlockHeight
  , blockTimeStrikeStrikeMediantime :: POSIXTime
  , blockTimeStrikeCreationTime     :: POSIXTime
  }
  deriving (Eq, Show, Generic)

newtype BlockTimeStrikeId = BlockTimeStrikeId
  { unBlockTimeStrikeId :: Word64
  }
  deriving (Eq, Show, Generic)

data BlockTimeStrikeObserved = BlockTimeStrikeObserved
  { judgementBlockMediantime :: POSIXTime -- mediantime of the judgement block.
  , judgementBlockHash :: BlockHash -- hash of the judgement block.
  , judgementBlockHeight :: BlockHeight -- height of the judgement block.
  , isFast :: SlowFast
  -- metadata
  , creationTime :: POSIXTime
  -- reflinks
  , strike :: BlockTimeStrikeId
  }
  deriving (Eq, Show, Generic)

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
  , blockTimeStrikeFilterObservedResultEQ           :: Maybe SlowFast
  , blockTimeStrikeFilterObservedResultNEQ          :: Maybe SlowFast
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
  , blockTimeStrikeFilterObservedResultEQ      = Just Slow
  , blockTimeStrikeFilterObservedResultNEQ     = Just Fast
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
  declareNamedSchema proxy = genericDeclareNamedSchema (commonSchemaOptions (def1 proxy)) proxy
    & mapped.schema.type_ ?~ SwaggerObject
    & mapped.schema.example ?~ toJSON defaultBlockTimeStrike
    & mapped.schema.required .~
      [ "block"
      , "strikeMediantime"
      , "creationTime"
      ]
    where
      def1 :: Default a => Proxy a -> a
      def1 _ = def
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
  declareNamedSchema proxy = genericDeclareNamedSchema (commonSchemaOptions (def1 proxy)) proxy
    & mapped.schema.type_ ?~ SwaggerObject
    & mapped.schema.example ?~ toJSON defaultBlockTimeStrikeObservedPublic
    & mapped.schema.required .~
      [ "blockHash"
      , "creationTime"
      , "result"
      , "blockMediantime"
      ]
    where
      def1 :: Default a => Proxy a -> a
      def1 _ = def
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
verifySlowFast v = error ("verifySlowFast: wrong value: " ++ Text.unpack v)

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
