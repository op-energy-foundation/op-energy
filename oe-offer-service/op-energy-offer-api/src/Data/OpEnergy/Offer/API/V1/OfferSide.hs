{-- | The maker's side of the bet — BEFORE or AFTER the MTP cutoff.
 -}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE OverloadedStrings          #-}
module Data.OpEnergy.Offer.API.V1.OfferSide
  ( OfferSide(..)
  , defaultOfferSide
  ) where

import           Data.Aeson
import           Data.Text                  (Text)
import           Data.Typeable              (Typeable)
import           GHC.Generics
import           Control.Lens               ((&), (?~))
import           Data.Swagger
import           Servant.API                (FromHttpApiData(..), ToHttpApiData(..))

-- | the maker's side of the bet
--
-- * @Before@ — maker wins if the real MTP is before the cutoff
--   (block was mined faster than predicted)
-- * @After@  — maker wins if the real MTP is after the cutoff
--   (block was mined slower than predicted)
data OfferSide
  = Before
  | After
  deriving (Show, Eq, Ord, Generic, Typeable, Enum, Bounded)

-- | uppercase serialisation for JSON and query params
offerSideToText :: OfferSide -> Text
offerSideToText Before = "BEFORE"
offerSideToText After  = "AFTER"

offerSideFromText :: Text -> Either Text OfferSide
offerSideFromText "BEFORE" = Right Before
offerSideFromText "AFTER"  = Right After
offerSideFromText other    = Left $ "OfferSide: unknown side: " <> other

instance ToJSON OfferSide where
  toJSON = toJSON . offerSideToText
instance FromJSON OfferSide where
  parseJSON = withText "OfferSide" $ either (fail . show) pure . offerSideFromText
instance ToSchema OfferSide where
  declareNamedSchema _ = pure $ NamedSchema (Just "OfferSide") $ mempty
    & type_ ?~ SwaggerString
    & enum_ ?~ map (toJSON . offerSideToText) [minBound .. maxBound]
    & example ?~ toJSON defaultOfferSide
instance ToParamSchema OfferSide where
  toParamSchema _ = mempty
    & type_ ?~ SwaggerString
    & enum_ ?~ map (toJSON . offerSideToText) [minBound .. maxBound]
instance FromHttpApiData OfferSide where
  parseQueryParam = offerSideFromText
instance ToHttpApiData OfferSide where
  toQueryParam = offerSideToText

defaultOfferSide :: OfferSide
defaultOfferSide = Before
