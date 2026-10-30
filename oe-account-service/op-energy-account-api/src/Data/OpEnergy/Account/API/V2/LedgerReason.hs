{-- | Closed set of reasons an account's balance can change.
 -
 - Every change of a balance is recorded with one of these, so the wallet's
 - transaction history can say what happened and the balance can be checked
 - against the sum of its entries.
 -}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE OverloadedStrings          #-}
module Data.OpEnergy.Account.API.V2.LedgerReason
  ( LedgerReason(..)
  , defaultLedgerReason
  , ledgerReasonToText
  , ledgerReasonFromText
  ) where

import           Data.Aeson
import           Data.Text                  (Text)
import           Data.Typeable              (Typeable)
import           GHC.Generics
import           Control.Lens               ((&), (?~))
import           Data.Swagger
import           Servant.API                (FromHttpApiData(..), ToHttpApiData(..))

-- | the closed set of reasons a balance changes
data LedgerReason
  = OpeningBalance -- ^ the balance an account is created with
  | Deposit -- ^ sats received over lightning
  | Withdrawal -- ^ sats sent over lightning
  | Stake -- ^ stake taken when posting or accepting an offer
  | Refund -- ^ stake returned when an offer is cancelled or expires
  | Winnings -- ^ payout of a settled contract
  deriving (Show, Eq, Ord, Generic, Typeable, Enum, Bounded)

-- | lowercase serialisation for JSON and query params
ledgerReasonToText :: LedgerReason -> Text
ledgerReasonToText OpeningBalance = "opening_balance"
ledgerReasonToText Deposit        = "deposit"
ledgerReasonToText Withdrawal     = "withdrawal"
ledgerReasonToText Stake          = "stake"
ledgerReasonToText Refund         = "refund"
ledgerReasonToText Winnings       = "winnings"

ledgerReasonFromText :: Text -> Either Text LedgerReason
ledgerReasonFromText "opening_balance" = Right OpeningBalance
ledgerReasonFromText "deposit"         = Right Deposit
ledgerReasonFromText "withdrawal"      = Right Withdrawal
ledgerReasonFromText "stake"           = Right Stake
ledgerReasonFromText "refund"          = Right Refund
ledgerReasonFromText "winnings"        = Right Winnings
ledgerReasonFromText other             =
  Left $ "LedgerReason: unknown reason: " <> other

defaultLedgerReason :: LedgerReason
defaultLedgerReason = Deposit

instance ToJSON LedgerReason where
  toJSON = toJSON . ledgerReasonToText
instance FromJSON LedgerReason where
  parseJSON = withText "LedgerReason"
    $ either (fail . show) pure . ledgerReasonFromText
instance ToSchema LedgerReason where
  declareNamedSchema _ = pure $ NamedSchema (Just "LedgerReason") $ mempty
    & type_ ?~ SwaggerString
    & enum_ ?~ map (toJSON . ledgerReasonToText) [minBound .. maxBound]
    & example ?~ toJSON defaultLedgerReason
instance ToParamSchema LedgerReason where
  toParamSchema _ = mempty
    & type_ ?~ SwaggerString
    & enum_ ?~ map (toJSON . ledgerReasonToText) [minBound .. maxBound]
instance FromHttpApiData LedgerReason where
  parseQueryParam = ledgerReasonFromText
instance ToHttpApiData LedgerReason where
  toQueryParam = ledgerReasonToText
