{-- | Contract data types: the response types for individual matched trades.
 -
 - A Contract represents one taker acceptance of an Offer slot. Fields
 - are denormalized from the parent Offer so contract listings do not
 - require joins.
 -}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DuplicateRecordFields      #-}
module Data.OpEnergy.Offer.API.V1.ContractInfo
  ( ContractID(..)
  , defaultContractID
  , ContractInfo(..)
  , defaultContractInfo
  ) where

import           Data.Swagger
import           Control.Lens
import           GHC.Generics
import           Data.Typeable              (Typeable)
import           Data.Aeson
import           Data.Text                  (Text)
import           Data.Time.Clock            (UTCTime)
import           Data.Word                  (Word64)
import           Servant.API                (FromHttpApiData(..), ToHttpApiData(..))

import           Data.OpEnergy.API.V1.Block (BlockHeight, defaultBlockHeight)
import           Data.OpEnergy.Account.API.V1.Account
                 ( DisplayName, defaultDisplayName
                 )
import           Data.OpEnergy.Offer.API.V1.OfferID
                 ( OfferID, defaultOfferID
                 )
import           Data.OpEnergy.Offer.API.V1.OfferSide
                 ( OfferSide, defaultOfferSide
                 )
import           Data.OpEnergy.Offer.API.V1.ContractStatus
                 ( ContractStatus, defaultContractStatus
                 )

-- | typed wrapper for contract identifiers
newtype ContractID = ContractID { unContractID :: Text }
  deriving (Show, Eq, Generic, Typeable)
instance ToJSON ContractID where
  toJSON (ContractID t) = toJSON t
instance FromJSON ContractID where
  parseJSON = withText "ContractID" $ pure . ContractID
instance ToSchema ContractID where
  declareNamedSchema _ = pure $ NamedSchema (Just "ContractID") $ mempty
    & type_ ?~ SwaggerString
    & example ?~ toJSON defaultContractID
instance ToParamSchema ContractID where
  toParamSchema _ = mempty
    & type_ ?~ SwaggerString
instance FromHttpApiData ContractID where
  parseQueryParam = Right . ContractID
instance ToHttpApiData ContractID where
  toQueryParam (ContractID t) = t

defaultContractID :: ContractID
defaultContractID = ContractID "1"

-- | one matched contract, as returned by accept/mine/list
data ContractInfo = ContractInfo
  { contractId       :: ContractID
  , offerId          :: OfferID
  , targetBlock      :: BlockHeight
  , mtpCutoffEpoch   :: Word64
  , makerSide        :: OfferSide
  , makerDisplayName :: DisplayName
  , makerStakeSats   :: Word64
  , takerDisplayName :: DisplayName
  , takerStakeSats   :: Word64
  , blockRate        :: Double
  , status           :: ContractStatus
  , confirmations    :: Word64
  , actualMtpEpoch   :: Maybe Word64
  , winnerSide       :: Maybe OfferSide
  , yourRole         :: Maybe Text  -- "maker" or "taker", present when auth'd
  , createdAtBlock   :: BlockHeight
  , matchedAt        :: UTCTime
  , settledAt        :: Maybe UTCTime
  }
  deriving (Show, Generic, Typeable)
instance ToJSON   ContractInfo
instance FromJSON ContractInfo
instance ToSchema ContractInfo where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "ContractInfo schema"
    & mapped.schema.example ?~ toJSON defaultContractInfo

defaultContractInfo :: ContractInfo
defaultContractInfo = ContractInfo
  { contractId = defaultContractID
  , offerId = defaultOfferID
  , targetBlock = defaultBlockHeight
  , mtpCutoffEpoch = 0
  , makerSide = defaultOfferSide
  , makerDisplayName = defaultDisplayName
  , makerStakeSats = 50000
  , takerDisplayName = defaultDisplayName
  , takerStakeSats = 50000
  , blockRate = 10.0
  , status = defaultContractStatus
  , confirmations = 0
  , actualMtpEpoch = Nothing
  , winnerSide = Nothing
  , yourRole = Nothing
  , createdAtBlock = defaultBlockHeight
  , matchedAt = read "2026-08-14 12:00:00 UTC"
  , settledAt = Nothing
  }
