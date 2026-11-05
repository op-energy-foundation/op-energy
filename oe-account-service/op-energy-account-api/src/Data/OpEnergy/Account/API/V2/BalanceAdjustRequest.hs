{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DuplicateRecordFields      #-}
module Data.OpEnergy.Account.API.V2.BalanceAdjustRequest
  ( BalanceAdjustRequest(..)
  , defaultBalanceAdjustRequest
  ) where

import           Data.Swagger
import           Data.Text                  (Text)
import           Control.Lens
import           GHC.Generics
import           Data.Typeable              (Typeable)
import           Data.Aeson

import           Data.OpEnergy.Account.API.V1.Account
                 ( Person
                 )
import           Data.OpEnergy.Account.API.V1.UUID
                 ( UUID, defaultUUID
                 )
import           Data.OpEnergy.Account.API.V1.Sats
                 ( Sats(..)
                 )
import           Data.OpEnergy.Account.API.V2.LedgerReason
                 ( LedgerReason
                 )

-- | body of the internal/balance/deduct and internal/balance/credit API
-- calls -- see those routes for what each does with it
data BalanceAdjustRequest = BalanceAdjustRequest
  { personUUID  :: UUID Person
  , amountSats  :: Sats
    -- ^ always given as a positive magnitude -- deduct subtracts it
    -- (failing rather than going negative), credit adds it
  , reason      :: Maybe LedgerReason
    -- ^ why the balance changes, recorded in the account's ledger and shown
    -- in its wallet history. Optional, so a caller, which predates the
    -- ledger, keeps working: deduct then records a stake, credit a refund
  , reference   :: Maybe Text
    -- ^ what the change relates to, eg "offer:42" or "contract:17"
  , counterparty :: Maybe (UUID Person)
    -- ^ the account on the other side of what is being credited, where
    -- there was one. Recorded with the entry, so a payout can be counted
    -- towards how many different accounts a winner has won against
  , freeSats    :: Maybe Sats
    -- ^ how much of 'amountSats' is free sats, which can be staked but
    -- never withdrawn. Credit returns a stake to the pocket it was paid
    -- from and pays a profit into the real one, so a payout of a stake,
    -- which was paid for with free sats, says so here. Deduct ignores it
    -- and spends free sats first, answering with what it took. Nothing
    -- means none, which is what every sat was before the two pockets
  }
  deriving (Show, Generic, Typeable)

defaultBalanceAdjustRequest :: BalanceAdjustRequest
defaultBalanceAdjustRequest = BalanceAdjustRequest
  defaultUUID (Sats 50000) Nothing Nothing Nothing Nothing

instance ToJSON BalanceAdjustRequest
instance FromJSON BalanceAdjustRequest
instance ToSchema BalanceAdjustRequest where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "BalanceAdjustRequest schema"
    & mapped.schema.example ?~ toJSON defaultBalanceAdjustRequest
