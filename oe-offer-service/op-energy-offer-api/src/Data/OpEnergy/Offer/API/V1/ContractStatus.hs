{-- | Closed set of states a Contract can be in.
 -
 - A contract is an individual matched trade between a maker and a
 - taker. The "confirming" stage (1–5 of 6 confirmations) is computed
 - at query time from the chain tip, not stored.
 -}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE OverloadedStrings          #-}
module Data.OpEnergy.Offer.API.V1.ContractStatus
  ( ContractStatus(..)
  , defaultContractStatus
  ) where

import           Data.Aeson
import           Data.Text                  (Text)
import           Data.Typeable              (Typeable)
import           GHC.Generics
import           Control.Lens               ((&), (?~))
import           Data.Swagger
import           Servant.API                (FromHttpApiData(..), ToHttpApiData(..))

-- | the closed set of states a contract can be in
data ContractStatus
  = Live
  | Settled
  deriving (Show, Eq, Ord, Generic, Typeable, Enum, Bounded)

-- | lowercase serialisation for JSON and query params
contractStatusToText :: ContractStatus -> Text
contractStatusToText Live    = "live"
contractStatusToText Settled = "settled"

contractStatusFromText :: Text -> Either Text ContractStatus
contractStatusFromText "live"    = Right Live
contractStatusFromText "settled" = Right Settled
contractStatusFromText other     = Left $ "ContractStatus: unknown status: " <> other

instance ToJSON ContractStatus where
  toJSON = toJSON . contractStatusToText
instance FromJSON ContractStatus where
  parseJSON = withText "ContractStatus" $ either (fail . show) pure . contractStatusFromText
instance ToSchema ContractStatus where
  declareNamedSchema _ = pure $ NamedSchema (Just "ContractStatus") $ mempty
    & type_ ?~ SwaggerString
    & enum_ ?~ map (toJSON . contractStatusToText) [minBound .. maxBound]
    & example ?~ toJSON defaultContractStatus
instance ToParamSchema ContractStatus where
  toParamSchema _ = mempty
    & type_ ?~ SwaggerString
    & enum_ ?~ map (toJSON . contractStatusToText) [minBound .. maxBound]
instance FromHttpApiData ContractStatus where
  parseQueryParam = contractStatusFromText
instance ToHttpApiData ContractStatus where
  toQueryParam = contractStatusToText

defaultContractStatus :: ContractStatus
defaultContractStatus = Live
