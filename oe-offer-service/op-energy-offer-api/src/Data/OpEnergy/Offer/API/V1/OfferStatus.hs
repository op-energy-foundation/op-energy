{-- | This module defines 'OfferStatus': the closed set of states an Offer
 - can be in. Phase 1 only ever produces "open", "expired" and "cancelled"
 - -- there is no matching engine yet, so "accepted"/"confirming"/"settled"
 - are declared up front but currently unreachable.
 -
 - PersistField/PersistFieldSql instances live in the service layer
 - (OpEnergy.Offer.Server.V1.Offer), not here -- per the project's
 - API-vs-Model separation convention.
 -}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE OverloadedStrings          #-}
module Data.OpEnergy.Offer.API.V1.OfferStatus
  ( OfferStatus
  , unOfferStatus
  , everifyOfferStatus
  , verifyOfferStatus
  , offerStatusOpen
  , offerStatusExpired
  , offerStatusCancelled
  , allOfferStatuses
  , defaultOfferStatus
  ) where

import           Data.Aeson
import           Data.Text                  (Text)
import           Data.Typeable              (Typeable)
import           GHC.Generics
import           Control.Lens               ((&), (?~))
import           Data.Swagger
import           Servant.API                (FromHttpApiData(..), ToHttpApiData(..))

newtype OfferStatus = OfferStatus { unOfferStatus :: Text }
  deriving (Show, Eq, Generic, Typeable)

-- | every status this schema has a place for
allOfferStatuses :: [Text]
allOfferStatuses = [ "open", "accepted", "expired", "cancelled", "confirming", "settled" ]

instance FromJSON OfferStatus where
  parseJSON = withText "OfferStatus" $ either (fail . show) pure . everifyOfferStatus
instance ToJSON OfferStatus where
  toJSON (OfferStatus t) = toJSON t
instance ToSchema OfferStatus where
  declareNamedSchema _ = pure $ NamedSchema (Just "OfferStatus") $ mempty
    & type_ ?~ SwaggerString
    & enum_ ?~ map toJSON allOfferStatuses
    & example ?~ toJSON defaultOfferStatus
instance ToParamSchema OfferStatus where
  toParamSchema _ = mempty
    & type_ ?~ SwaggerString
    & enum_ ?~ map toJSON allOfferStatuses
instance FromHttpApiData OfferStatus where
  parseQueryParam = everifyOfferStatus
instance ToHttpApiData OfferStatus where
  toQueryParam (OfferStatus t) = t

everifyOfferStatus :: Text -> Either Text OfferStatus
everifyOfferStatus raw
  | raw `elem` allOfferStatuses = Right (OfferStatus raw)
  | otherwise = Left "OfferStatus: must be one of open/accepted/expired/cancelled/confirming/settled"

-- | partial version of 'everifyOfferStatus'. Only for use on values already
-- known to be well-formed.
verifyOfferStatus :: Text -> OfferStatus
verifyOfferStatus raw = case everifyOfferStatus raw of
  Right ret -> ret
  Left some -> error (show some)

offerStatusOpen, offerStatusExpired, offerStatusCancelled :: OfferStatus
offerStatusOpen = verifyOfferStatus "open"
offerStatusExpired = verifyOfferStatus "expired"
offerStatusCancelled = verifyOfferStatus "cancelled"

defaultOfferStatus :: OfferStatus
defaultOfferStatus = offerStatusOpen
