{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DuplicateRecordFields      #-}
module Data.OpEnergy.Account.API.V2.AccountInfo
  ( AccountInfo(..)
  , defaultAccountInfo
  ) where

import           Data.Swagger
import           Control.Lens
import           GHC.Generics
import           Data.Typeable              (Typeable)
import           Data.Aeson

import           Data.OpEnergy.Account.API.V1.Account
                 ( DisplayName
                 , defaultDisplayName
                 )
import           Data.OpEnergy.Account.API.V1.Sats
                 ( Sats
                 , defaultSats
                 )
import           Data.OpEnergy.Account.API.V2.PersonRole
                 ( PersonRole
                 , defaultPersonRole
                 )

-- | result of the 'me' API call — identity, spendable balance and what the
-- account is allowed to do
data AccountInfo = AccountInfo
  { displayName :: DisplayName
  , hasPassword :: Bool
  , balance     :: Sats
  , role        :: PersonRole
    -- ^ what this account may do beyond acting for itself. The UI shows the
    -- moderator controls by it; the service enforces it regardless
  }
  deriving (Show, Generic, Typeable)

defaultAccountInfo :: AccountInfo
defaultAccountInfo =
  AccountInfo defaultDisplayName False defaultSats defaultPersonRole

instance ToJSON AccountInfo
instance FromJSON AccountInfo
instance ToSchema AccountInfo where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "AccountInfo schema"
    & mapped.schema.example ?~ toJSON defaultAccountInfo
