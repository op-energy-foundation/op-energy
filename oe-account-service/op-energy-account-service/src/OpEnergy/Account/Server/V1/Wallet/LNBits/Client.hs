{-- | The routes and payloads of the LNBits HTTP API, as the lnbits wallet
 - backend speaks them.
 -
 - This module describes what LNBits accepts and answers, and nothing about
 - what this service makes of it: the wallet backend's own types, and the
 - millisatoshi amounts LNBits answers with, are handled in
 - "OpEnergy.Account.Server.V1.Wallet.LNBits".
 -
 - Every route carries the wallet's API key in a header, so a rendered
 - request carries the key as well. Nothing here is ever put into an error
 - or a log line for that reason.
 -}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}
module OpEnergy.Account.Server.V1.Wallet.LNBits.Client
  ( CreateInvoiceRequest(..)
  , PaymentResponse(..)
  , PaymentStateResponse(..)
  , createInvoice
  , readPaymentState
  ) where

import           Data.Aeson
                 ( FromJSON(..), ToJSON(..), object, withObject
                 , (.=), (.:), (.:?), (.!=)
                 )
import           Data.Int (Int64)
import           Data.Proxy (Proxy(..))
import           Data.Text (Text)
import           Data.Word (Word64)
import           Servant.API
import           Servant.Client (ClientM, client)

-- | an invoice, which LNBits is asked to issue. The amount is in satoshi,
-- which is the unit a request carries, while an answer reports
-- millisatoshi.
--
-- There is no field for the direction: this request is only ever sent to
-- be paid to this wallet, so "out" is fixed to False below and a payment
-- can not be made by filling this in differently
data CreateInvoiceRequest = CreateInvoiceRequest
  { createInvoiceRequestAmountSats :: Word64
  , createInvoiceRequestMemo :: Maybe Text
  , createInvoiceRequestExpirySecs :: Word64
  }
  deriving (Show, Eq)

instance ToJSON CreateInvoiceRequest where
  toJSON request = object
    [ "out" .= False
    , "amount" .= createInvoiceRequestAmountSats request
    , "memo" .= createInvoiceRequestMemo request
    , "expiry" .= createInvoiceRequestExpirySecs request
    ]

-- | a payment as LNBits reports it.
--
-- The amounts are in millisatoshi and are negative for a payment, which
-- left the wallet, so they are Int64 here and converted where they are
-- used. The status is one of LNBits' own words for it, carried as it
-- arrived rather than parsed here
data PaymentResponse = PaymentResponse
  { paymentResponsePaymentHash :: Text
  , paymentResponseBolt11 :: Text
  , paymentResponseAmountMsats :: Int64
  , paymentResponseFeeMsats :: Int64
  , paymentResponseStatus :: Text
  }
  deriving (Show, Eq)

instance FromJSON PaymentResponse where
  parseJSON = withObject "PaymentResponse" $ \v-> PaymentResponse
    <$> v .: "payment_hash"
    <*> v .: "bolt11"
    <*> v .: "amount"
    <*> v .:? "fee" .!= 0
    -- an answer without a status is one, which has not been paid yet:
    -- LNBits leaves the field out until there is something to report
    <*> v .:? "status" .!= "pending"

-- | what LNBits answers about a payment it is given the hash of. 'paid' is
-- the explicit answer and 'status' describes how it got there
data PaymentStateResponse = PaymentStateResponse
  { paymentStateResponsePaid :: Bool
  , paymentStateResponseStatus :: Text
  }
  deriving (Show, Eq)

instance FromJSON PaymentStateResponse where
  parseJSON = withObject "PaymentStateResponse" $ \v-> PaymentStateResponse
    <$> v .:? "paid" .!= False
    <*> v .:? "status" .!= "pending"

-- | LNBits answers a created invoice with 201, not 200
type CreateInvoiceAPI
  = "api" :> "v1" :> "payments"
    :> Header' '[Required, Strict] "X-Api-Key" Text
    :> ReqBody '[JSON] CreateInvoiceRequest
    :> PostCreated '[JSON] PaymentResponse

type ReadPaymentStateAPI
  = "api" :> "v1" :> "payments"
    :> Header' '[Required, Strict] "X-Api-Key" Text
    :> Capture "payment_hash" Text
    :> Get '[JSON] PaymentStateResponse

-- | asks LNBits for an invoice to be paid to the wallet the key names
createInvoice :: Text -> CreateInvoiceRequest -> ClientM PaymentResponse
createInvoice = client (Proxy :: Proxy CreateInvoiceAPI)

-- | asks LNBits what has become of the payment with the given hash
readPaymentState :: Text -> Text -> ClientM PaymentStateResponse
readPaymentState = client (Proxy :: Proxy ReadPaymentStateAPI)
