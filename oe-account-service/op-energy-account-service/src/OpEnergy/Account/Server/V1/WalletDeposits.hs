{-- | Crediting of an incoming payment, exactly once.
 -
 - Marking an invoice settled and crediting the balance for it are separate
 - transactions, and more than one path can arrive at the same settled
 - payment: the settle handler, a retry of it, and -- once the wallet is
 - asked what it has settled -- the scheduler. 'creditDepositOnce' is what
 - makes the credit happen once between them: it claims the payment by
 - writing its creditedAt, and only the claim, which wins, moves the
 - balance.
 -
 - Claiming through the same condition is also what recovers a credit, which
 - was lost: a payment, which is settled but whose creditedAt is unset, is
 - still waiting to be credited, so the next caller credits it.
 -}
{-# LANGUAGE OverloadedStrings          #-}
module OpEnergy.Account.Server.V1.WalletDeposits
  ( creditDepositOnce
  ) where

import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans (lift)
import           Control.Monad.Trans.Except (ExceptT(..), runExceptT)
import           Control.Monad.Trans.Reader (ReaderT)
import           Data.Int (Int64)
import           Data.Time.Clock.POSIX (POSIXTime)
import           Database.Persist
import           Database.Persist.Sql (SqlBackend, updateWhereCount)

import           Data.OpEnergy.Account.API.V1.Sats (Sats(..))
import           Data.OpEnergy.Account.API.V2.LedgerReason (LedgerReason(..))
import           Data.OpEnergy.Account.API.V2.PaymentHash (PaymentHash(..))

import           OpEnergy.Account.Server.V1.LedgerEntry
                 ( LedgerDirection(..), adjustBalanceTx, ledgerReference
                 )
import           OpEnergy.Account.Server.V1.WalletPayment
import           OpEnergy.Error
                 ( CallstackError, walletInvoiceNotFound
                 )
import           OpEnergy.ExceptMaybe (exceptTMaybeT)

-- | credits the account of an incoming payment, unless it has been credited
-- already. Returns the balance it produced, or Nothing when somebody else
-- had credited it first.
--
-- The claim and the balance move happen in one transaction, and the claim
-- only succeeds while creditedAt is unset, so two callers arriving at the
-- same payment together credit it once between them. Only an incoming
-- payment is ever credited: an outgoing one is money, which has left.
--
-- Example:
--
-- > creditDepositOnce paymentHash now
creditDepositOnce
  :: MonadIO m
  => PaymentHash
  -> POSIXTime
  -> ReaderT SqlBackend m (Either CallstackError (Maybe Sats))
creditDepositOnce paymentHashV now = runExceptT $ do
  claimed <- lift $ updateWhereCount
    [ WalletPaymentPaymentHash ==. paymentHashV
    , WalletPaymentDirection ==. Incoming
    , WalletPaymentCreditedAt ==. Nothing
    ]
    [ WalletPaymentCreditedAt =. Just now
    , WalletPaymentUpdatedAt =. now
    ]
  if claimed /= (1 :: Int64)
    then return Nothing
    else do
      (Entity _ payment) <- exceptTMaybeT walletInvoiceNotFound
        $ selectFirst [ WalletPaymentPaymentHash ==. paymentHashV ] []
      balance <- ExceptT $ adjustBalanceTx
        (walletPaymentPersonId payment)
        Credit
        (walletPaymentAmountSats payment)
        Deposit
        (Just (ledgerReference "payment" (unPaymentHash paymentHashV)))
        now
      return $! Just balance
