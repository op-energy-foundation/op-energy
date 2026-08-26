{-- | This module defines 'OfferStatus': the closed set of states an Offer
 - can be in. Phase 1 (see docs/plans/post-offer-api.md) only ever produces
 - "open", "expired" and "cancelled" -- there is no matching engine yet, so
 - "accepted"/"confirming"/"settled" are declared up front (this is the
 - schema the feature was speced against) but currently unreachable.
 -
 - A closed wire type here -- rather than a raw Text, as
 - GET .../offer/list's "status" filter would otherwise be -- means a bad
 - filter value is rejected by Servant's own query-param parsing
 - (FromHttpApiData) before a handler ever runs, the same way Servant
 - already rejects a malformed DisplayName or AccountToken.
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

import           Database.Persist
import           Database.Persist.Sql

newtype OfferStatus = OfferStatus { unOfferStatus :: Text }
  deriving (Show, Eq, Generic, Typeable)

-- | every status this schema has a place for -- see module header for which
-- ones Phase 1 can actually produce
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

instance PersistField OfferStatus where
  toPersistValue (OfferStatus t) = toPersistValue t
  fromPersistValue (PersistText t) = either (Left . ("OfferStatus.hs fromPersistValue: " <>)) Right
    $! everifyOfferStatus t
  fromPersistValue _ = Left "OfferStatus.hs fromPersistValue, expected Text"
instance PersistFieldSql OfferStatus where
  sqlType _ = SqlString

everifyOfferStatus :: Text -> Either Text OfferStatus
everifyOfferStatus raw
  | raw `elem` allOfferStatuses = Right (OfferStatus raw)
  | otherwise = Left "OfferStatus: must be one of open/accepted/expired/cancelled/confirming/settled"

-- | partial version of 'everifyOfferStatus'. Only for use on values already
-- known to be well-formed (this module's own constants below, or a value
-- already round-tripped through the DB), as it calls 'error' otherwise.
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
