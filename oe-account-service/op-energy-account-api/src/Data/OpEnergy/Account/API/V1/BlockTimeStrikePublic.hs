{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DeriveGeneric              #-}
module Data.OpEnergy.Account.API.V1.BlockTimeStrikePublic where

import           Data.Swagger
import           Control.Lens
import           GHC.Generics
import           Data.Aeson as A
import           Data.Word
import           Data.List as List
import           Data.Char as Char
import           Data.OpEnergy.Account.API.V1.BlockTimeStrike
import           Data.Default

data BlockTimeStrikePastPublic = BlockTimeStrikePastPublic
  { pastStrike :: BlockTimeStrikePast
  , guessesCount :: Word32
  }
  deriving (Show, Generic)
instance FromJSON BlockTimeStrikePastPublic
instance ToJSON BlockTimeStrikePastPublic where
  toJSON = genericToJSON defaultOptions
    { A.fieldLabelModifier =
      (\s -> case s of
          [] -> []
          (h:t) -> (Char.toLower h):t
      )
    , A.constructorTagModifier = List.map Char.toLower
    }
  toEncoding = genericToEncoding defaultOptions
    { A.fieldLabelModifier =
      (\s -> case s of
          [] -> []
          (h:t) -> (Char.toLower h):t
      )
    , A.constructorTagModifier = List.map Char.toLower
    }
instance ToSchema BlockTimeStrikePastPublic where
  declareNamedSchema _ = return $ NamedSchema (Just "BlockTimeStrikePastPublic") $ mempty
    & type_ ?~ SwaggerObject
    & example ?~ toJSON defaultBlockTimeStrikePastPublic
instance Default BlockTimeStrikePastPublic where
  def = defaultBlockTimeStrikePastPublic
defaultBlockTimeStrikePastPublic :: BlockTimeStrikePastPublic
defaultBlockTimeStrikePastPublic = BlockTimeStrikePastPublic
  { pastStrike = defaultBlockTimeStrikePast
  , guessesCount = 0
  }
