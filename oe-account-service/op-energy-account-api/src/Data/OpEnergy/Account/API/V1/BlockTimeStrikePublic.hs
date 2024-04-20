{-- | This module defines BlockTimeStrikePastPublic datatype
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

data BlockTimeStrikePastPublic = BlockTimeStrikePastPublic
  { blockTimeStrikePastPublicPastStrike :: BlockTimeStrikePast
    -- ^ past strike
  , blockTimeStrikePastPublicGuessesCount :: Word32
    -- ^ amount of guesses
  }
  deriving (Show, Generic)
instance FromJSON BlockTimeStrikePastPublic where
  parseJSON = commonParseJSON
instance ToJSON BlockTimeStrikePastPublic where
  -- alter serialization names
  toJSON = commonToJSON genericToJSON
  toEncoding = commonToJSON genericToEncoding
instance ToSchema BlockTimeStrikePastPublic where
  declareNamedSchema _ = return $ NamedSchema (Just "BlockTimeStrikePastPublic") $ mempty
    & type_ ?~ SwaggerObject
    & example ?~ toJSON defaultBlockTimeStrikePastPublic
instance Default BlockTimeStrikePastPublic where
  def = defaultBlockTimeStrikePastPublic
defaultBlockTimeStrikePastPublic :: BlockTimeStrikePastPublic
defaultBlockTimeStrikePastPublic = BlockTimeStrikePastPublic
  { blockTimeStrikePastPublicPastStrike = defaultBlockTimeStrikePast
  , blockTimeStrikePastPublicGuessesCount = 0
  }
