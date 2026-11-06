{-- | What a client needs to show the wallet: the balance, which wallet is
 - active and what it currently allows.
 -}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE OverloadedStrings          #-}
module Data.OpEnergy.Account.API.V2.WalletInfo
  ( WalletInfo(..)
  , defaultWalletInfo
  ) where

import           Data.Swagger
import           Control.Lens
import           GHC.Generics
import           Data.Typeable              (Typeable)
import           Data.Aeson

import           Data.OpEnergy.Account.API.V1.Sats (Sats(..))
import           Data.OpEnergy.Account.API.V2.WalletMode
                 ( WalletMode(..), defaultWalletMode
                 )

-- | state of an account's wallet
data WalletInfo = WalletInfo
  { balanceSats :: Sats
  , freeSats :: Sats
    -- ^ how much of the balance was given rather than paid in. Free sats
    -- can be staked, and can be won and lost, but can never be sent
  , giftWonSats :: Sats
    -- ^ how much of the balance the gift won: real sats, which can be sent
    -- once the withdrawal limits are cleared, and not before
  , withdrawableSats :: Sats
    -- ^ what can actually be sent: the balance less its free sats
  , mode :: WalletMode
    -- ^ "mock" means no real sats move
  , canSend :: Bool
  , canReceive :: Bool
  , minInvoiceSats :: Sats
  , maxInvoiceSats :: Sats
  , maxWithdrawalSats :: Sats
  }
  deriving (Show, Generic, Typeable)

defaultWalletInfo :: WalletInfo
defaultWalletInfo = WalletInfo
  { balanceSats = Sats 300000
  , freeSats = Sats 300000
  , giftWonSats = Sats 0
  , withdrawableSats = Sats 0
  , mode = defaultWalletMode
  , canSend = True
  , canReceive = True
  , minInvoiceSats = Sats 1
  , maxInvoiceSats = Sats 10000000
  , maxWithdrawalSats = Sats 10000000
  }

instance ToJSON WalletInfo
instance FromJSON WalletInfo
instance ToSchema WalletInfo where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "WalletInfo schema"
    & mapped.schema.example ?~ toJSON defaultWalletInfo
