{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DuplicateRecordFields      #-}
module Data.OpEnergy.Account.API.V2.WhoAmIResult
  ( WhoAmIResult(..)
  , defaultWhoAmIResult
  ) where

import           Data.Swagger
import           Control.Lens
import           GHC.Generics
import           Data.Typeable              (Typeable)
import           Data.Aeson

import           Data.OpEnergy.Account.API.V1.Account
                 ( Person, DisplayName, defaultDisplayName
                 )
import           Data.OpEnergy.Account.API.V1.UUID
                 ( UUID, defaultUUID
                 )
import           Data.OpEnergy.Account.API.V1.Sats
                 ( Sats(..)
                 )
import           Data.OpEnergy.Account.API.V2.PersonRole
                 ( PersonRole
                 , defaultPersonRole
                 )

-- | resolves an account token to the account's stable cross-service
-- identity (personUUID), current display name, current sandbox wallet
-- balance, how much of that balance is free sats, and what the account is
-- allowed to do
data WhoAmIResult = WhoAmIResult
  { personUUID   :: UUID Person
  , displayName  :: DisplayName
  , balance      :: Sats
  , freeSats     :: Sats
    -- ^ how much of 'balance' was given rather than paid in: it can be
    -- staked, and can be won and lost, but can never be withdrawn
  , role         :: PersonRole
    -- ^ what this account may do beyond acting for itself. Carried here so
    -- a service, which already resolves a token, learns it without a
    -- second call
  }
  deriving (Show, Generic, Typeable)

defaultWhoAmIResult :: WhoAmIResult
defaultWhoAmIResult =
  WhoAmIResult defaultUUID defaultDisplayName (Sats 300000) (Sats 300000)
    defaultPersonRole

instance ToJSON WhoAmIResult
instance FromJSON WhoAmIResult
instance ToSchema WhoAmIResult where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "WhoAmIResult schema"
    & mapped.schema.example ?~ toJSON defaultWhoAmIResult
