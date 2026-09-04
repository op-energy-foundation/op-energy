{-- | Closed set of states an Offer can be in.
 -}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE OverloadedStrings          #-}
module Data.OpEnergy.Offer.API.V1.OfferStatus
  ( OfferStatus(..)
  , defaultOfferStatus
  ) where

import           Data.Aeson
import           Data.Text                  (Text)
import           Data.Typeable              (Typeable)
import           GHC.Generics
import           Control.Lens               ((&), (?~))
import           Data.Swagger
import           Servant.API                (FromHttpApiData(..), ToHttpApiData(..))

-- | the closed set of states an offer can be in
data OfferStatus
  = Open
  | Accepted
  | Expired
  | Cancelled
  | Confirming
  | Settled
  deriving (Show, Eq, Ord, Generic, Typeable, Enum, Bounded)

-- | lowercase serialisation for JSON and query params
offerStatusToText :: OfferStatus -> Text
offerStatusToText Open       = "open"
offerStatusToText Accepted   = "accepted"
offerStatusToText Expired    = "expired"
offerStatusToText Cancelled  = "cancelled"
offerStatusToText Confirming = "confirming"
offerStatusToText Settled    = "settled"

offerStatusFromText :: Text -> Either Text OfferStatus
offerStatusFromText "open"       = Right Open
offerStatusFromText "accepted"   = Right Accepted
offerStatusFromText "expired"    = Right Expired
offerStatusFromText "cancelled"  = Right Cancelled
offerStatusFromText "confirming" = Right Confirming
offerStatusFromText "settled"    = Right Settled
offerStatusFromText other        = Left $ "OfferStatus: unknown status: " <> other

instance ToJSON OfferStatus where
  toJSON = toJSON . offerStatusToText
instance FromJSON OfferStatus where
  parseJSON = withText "OfferStatus" $ either (fail . show) pure . offerStatusFromText
instance ToSchema OfferStatus where
  declareNamedSchema _ = pure $ NamedSchema (Just "OfferStatus") $ mempty
    & type_ ?~ SwaggerString
    & enum_ ?~ map (toJSON . offerStatusToText) [minBound .. maxBound]
    & example ?~ toJSON defaultOfferStatus
instance ToParamSchema OfferStatus where
  toParamSchema _ = mempty
    & type_ ?~ SwaggerString
    & enum_ ?~ map (toJSON . offerStatusToText) [minBound .. maxBound]
instance FromHttpApiData OfferStatus where
  parseQueryParam = offerStatusFromText
instance ToHttpApiData OfferStatus where
  toQueryParam = offerStatusToText

defaultOfferStatus :: OfferStatus
defaultOfferStatus = Open
