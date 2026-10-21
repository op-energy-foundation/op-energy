{-- | What a payment cost and what it left in the account.
 -}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE OverloadedStrings          #-}
module Data.OpEnergy.Account.API.V2.PayInvoiceResult
  ( PayInvoiceResult(..)
  , defaultPayInvoiceResult
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

-- | a payment, which has been made
data PayInvoiceResult = PayInvoiceResult
  { paymentHash :: PaymentHash
  , amountSats :: Sats
  , feeSats :: Sats
    -- ^ what the payment cost, once it is known. A payment, which is
    -- still in flight, has not been charged a fee yet: this is then what
    -- was reserved for one, which is the most it can cost
  , balanceSats :: Sats
    -- ^ balance after the payment, so a client needs no second call
  , settled :: Bool
    -- ^ whether the wallet is done with the payment. False for one, which
    -- it has taken and is still trying: the sats are held, the fee is not
    -- final, and the balance is the one, which the holding left. Such a
    -- payment is finished by this service when the wallet says what became
    -- of it, and the account's history is what reports that
  }
  deriving (Show, Generic, Typeable)

defaultPayInvoiceResult :: PayInvoiceResult
defaultPayInvoiceResult = PayInvoiceResult
  defaultPaymentHash (Sats 50000) (Sats 0) (Sats 250000) True

instance ToJSON PayInvoiceResult
instance FromJSON PayInvoiceResult
instance ToSchema PayInvoiceResult where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "PayInvoiceResult schema"
    & mapped.schema.example ?~ toJSON defaultPayInvoiceResult
