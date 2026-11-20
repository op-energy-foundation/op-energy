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
import qualified Data.Text as Text
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
  }
  deriving (Show, Generic, Typeable)

defaultBalanceAdjustRequest :: BalanceAdjustRequest
defaultBalanceAdjustRequest = BalanceAdjustRequest
  defaultUUID (Sats 50000) Nothing Nothing

-- | the longest reference a caller may send. A reference names a record of
-- the calling service, eg "offer:42", so this is far above anything, which
-- describes one: the column is unbounded and the whole body is read into
-- memory before a handler sees it, so without a bound one call stores as
-- much as it is sent
maxReferenceLength :: Int
maxReferenceLength = 128

-- | checks the length of a reference, as deserialisation is where a value
-- is checked
everifyReference :: Maybe Text -> Either Text (Maybe Text)
everifyReference Nothing = Right Nothing
everifyReference (Just raw)
  | Text.length raw > maxReferenceLength =
      Left "BalanceAdjustRequest: reference is longer than allowed"
  | otherwise = Right (Just raw)

instance ToJSON BalanceAdjustRequest
instance FromJSON BalanceAdjustRequest where
  parseJSON = withObject "BalanceAdjustRequest" $ \v-> do
    personUUIDV <- v .: "personUUID"
    amountSatsV <- v .: "amountSats"
    reasonV <- v .:? "reason"
    referenceV <- v .:? "reference"
    -- positional, as several request types share these field names
    either (fail . Text.unpack)
      (return . BalanceAdjustRequest personUUIDV amountSatsV reasonV)
      $ everifyReference referenceV
instance ToSchema BalanceAdjustRequest where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "BalanceAdjustRequest schema"
    & mapped.schema.example ?~ toJSON defaultBalanceAdjustRequest
