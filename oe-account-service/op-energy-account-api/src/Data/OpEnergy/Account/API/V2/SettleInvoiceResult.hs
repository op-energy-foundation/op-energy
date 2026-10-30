{-- | What settling a mock invoice left in the account.
 -}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE OverloadedStrings          #-}
module Data.OpEnergy.Account.API.V2.SettleInvoiceResult
  ( SettleInvoiceResult(..)
  , defaultSettleInvoiceResult
  ) where

import           Data.Swagger
import           Control.Lens
import           GHC.Generics
import           Data.Typeable              (Typeable)
import           Data.Aeson

import           Data.OpEnergy.Account.API.V1.Sats (Sats(..))
import           Data.OpEnergy.Account.API.V2.PaymentHash
                 ( PaymentHash(..), defaultPaymentHash
                 )

-- | an invoice, which is now paid
data SettleInvoiceResult = SettleInvoiceResult
  { paymentHash :: PaymentHash
  , amountSats :: Sats
  , balanceSats :: Sats
    -- ^ balance after the payment. Settling an invoice, which was already
    -- paid, changes nothing and reports the same balance again
  }
  deriving (Show, Generic, Typeable)

defaultSettleInvoiceResult :: SettleInvoiceResult
defaultSettleInvoiceResult = SettleInvoiceResult
  defaultPaymentHash (Sats 50000) (Sats 350000)

instance ToJSON SettleInvoiceResult
instance FromJSON SettleInvoiceResult
instance ToSchema SettleInvoiceResult where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "SettleInvoiceResult schema"
    & mapped.schema.example ?~ toJSON defaultSettleInvoiceResult
