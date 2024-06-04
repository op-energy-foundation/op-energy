{-- | This module defines BlockTimeStrikePublic datatype
 --}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DeriveGeneric              #-}
module Data.OpEnergy.Account.API.V1.BlockTimeStrikePublic where

import           Data.Swagger
import           Control.Lens
import           GHC.Generics
import           Data.Aeson as A
import           Data.Word
import           Data.OpEnergy.Account.API.V1.BlockTimeStrike
import           Data.OpEnergy.Account.API.V1.Common
import           Data.Default

data BlockTimeStrikePublic = BlockTimeStrikePublic
  { blockTimeStrikePublicStrike :: BlockTimeStrike
    -- ^ past strike
  , blockTimeStrikePublicGuessesCount :: Word32
    -- ^ amount of guesses
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
defaultBlockTimeStrikePublic = BlockTimeStrikePublic
  { blockTimeStrikePublicStrike = defaultBlockTimeStrike
  , blockTimeStrikePublicGuessesCount = 0
  }
