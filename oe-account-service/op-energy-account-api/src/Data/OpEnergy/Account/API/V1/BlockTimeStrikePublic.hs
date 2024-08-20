{-- | This module defines BlockTimeStrikeWithGuessesCountPublic datatype
 --}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DeriveGeneric              #-}
module Data.OpEnergy.Account.API.V1.BlockTimeStrikePublic where

import           Data.Swagger
import           Control.Lens
import           GHC.Generics
import           Data.Aeson as A
import           Data.Word
import           Data.Time.Clock.POSIX(POSIXTime)

import           Data.OpEnergy.API.V1.Block(BlockHash)
import qualified Data.OpEnergy.API.V1.Hash as BlockHash (defaultHash)
import           Data.OpEnergy.API.V1.Block(BlockHeight, defaultBlockHeight)
import           Data.OpEnergy.Account.API.V1.BlockTimeStrike
import           Data.OpEnergy.Account.API.V1.Common
import           Data.Default

data BlockTimeStrikePublic = BlockTimeStrikePublic
  { blockTimeStrikePublicBlock :: BlockHeight
  , blockTimeStrikePublicStrikeMediantime :: POSIXTime
  , blockTimeStrikePublicCreationTime :: POSIXTime
  , blockTimeStrikePublicObservedResult :: Maybe SlowFast
  , blockTimeStrikePublicObservedBlockMediantime :: Maybe POSIXTime
  , blockTimeStrikePublicObservedBlockHash :: Maybe BlockHash
  , blockTimeStrikePublicObservedBlockHeight :: Maybe BlockHeight
  }
  deriving (Show, Generic)
instance FromJSON BlockTimeStrikePublic where
  parseJSON = commonParseJSON
instance ToJSON BlockTimeStrikePublic where
  -- alter serialization names
  toJSON = commonToJSON genericToJSON
  toEncoding = commonToJSON genericToEncoding
instance ToSchema BlockTimeStrikePublic where
  declareNamedSchema _ = return $ NamedSchema (Just "BlockTimeStrikePublic") $ mempty
    & type_ ?~ SwaggerObject
    & example ?~ toJSON defaultBlockTimeStrikePublic
instance Default BlockTimeStrikePublic where
  def = defaultBlockTimeStrikePublic
defaultBlockTimeStrikePublic :: BlockTimeStrikePublic
defaultBlockTimeStrikePublic =  BlockTimeStrikePublic
  { blockTimeStrikePublicBlock = defaultBlockHeight
  , blockTimeStrikePublicStrikeMediantime = 2
  , blockTimeStrikePublicCreationTime = 1
  , blockTimeStrikePublicObservedResult = Just defaultSlowFast
  , blockTimeStrikePublicObservedBlockMediantime = Just 3
  , blockTimeStrikePublicObservedBlockHash = Just BlockHash.defaultHash
  , blockTimeStrikePublicObservedBlockHeight = Just defaultBlockHeight
  }

data BlockTimeStrikeWithGuessesCountPublic = BlockTimeStrikeWithGuessesCountPublic
  { blockTimeStrikeWithGuessesCountPublicStrike :: BlockTimeStrikePublic
    -- ^ past strike
  , blockTimeStrikeWithGuessesCountPublicGuessesCount :: Word32
    -- ^ amount of guesses
  }
  deriving (Show, Generic)
instance FromJSON BlockTimeStrikeWithGuessesCountPublic where
  parseJSON = commonParseJSON
instance ToJSON BlockTimeStrikeWithGuessesCountPublic where
  -- alter serialization names
  toJSON = commonToJSON genericToJSON
  toEncoding = commonToJSON genericToEncoding
instance ToSchema BlockTimeStrikeWithGuessesCountPublic where
  declareNamedSchema _ = return $ NamedSchema (Just "BlockTimeStrikeWithGuessesCountPublic") $ mempty
    & type_ ?~ SwaggerObject
    & example ?~ toJSON defaultBlockTimeStrikeWithGuessesCountPublic
instance Default BlockTimeStrikeWithGuessesCountPublic where
  def = defaultBlockTimeStrikeWithGuessesCountPublic
defaultBlockTimeStrikeWithGuessesCountPublic :: BlockTimeStrikeWithGuessesCountPublic
defaultBlockTimeStrikeWithGuessesCountPublic = BlockTimeStrikeWithGuessesCountPublic
  { blockTimeStrikeWithGuessesCountPublicStrike = defaultBlockTimeStrikePublic
  , blockTimeStrikeWithGuessesCountPublicGuessesCount = 0
  }
