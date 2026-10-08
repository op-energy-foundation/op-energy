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
{-# LANGUAGE TemplateHaskell            #-}
module OpEnergy.Account.Server.V1.WalletDeposits
  ( creditDepositOnce
  , creditWaitingDeposits
  , maxDepositsPerTick
  ) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Logger (logError)
import           Control.Monad.Trans (lift)
import           Control.Monad.Trans.Except (ExceptT(..), runExceptT)
import           Control.Monad.Trans.Reader (ReaderT, ask)
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           Database.Persist.Postgresql (runSqlPersistMPool)
import           Prometheus (MonadMonitor)
import           Data.Int (Int64)
import           Data.Time.Clock.POSIX (POSIXTime)
import           Database.Persist
import           Database.Persist.Sql (SqlBackend, updateWhereCount)

import           Data.OpEnergy.Account.API.V1.Sats (Sats(..))
import           Data.OpEnergy.Account.API.V2.LedgerReason (LedgerReason(..))
import           Data.OpEnergy.Account.API.V2.PaymentHash (PaymentHash(..))

import           OpEnergy.Account.Server.V1.Class
                 ( AppT, State(..), profile, runLogging
                 )
import           OpEnergy.Account.Server.V1.LedgerEntry
                 ( LedgerDirection(..), adjustBalanceTx, ledgerReference
                 )
import           OpEnergy.Account.Server.V1.WalletPayment
import           OpEnergy.Error
                 ( CallstackError, describeError, eitherException
                 , walletInvoiceNotFound
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

-- | how many waiting deposits one tick credits, so the work a tick does is
-- bounded whatever has accumulated
maxDepositsPerTick :: Int
maxDepositsPerTick = 100

-- | credits the incoming payments, which are settled and not yet credited.
-- Run from the scheduler.
--
-- Marking an invoice settled and crediting the balance for it are separate
-- transactions, so a credit can be lost between them -- a pool failure, the
-- process going down. Until now only a retry of the settle recovered one,
-- which needs somebody to retry; this credits them without being asked.
--
-- It asks this service's own database which payments are waiting, rather
-- than asking the wallet what it has settled since a remembered time. That
-- is deliberate: "settled, creditedAt unset" is exactly the condition a
-- credit is owed for, so there is no window to miss and nothing to
-- remember, and a restart resumes with the same answer. Asking a wallet
-- what it settled since a time is a different job -- finding payments this
-- service was never told about -- and belongs with the backend, which can
-- answer it.
creditWaitingDeposits :: (MonadIO m, MonadMonitor m) => AppT m ()
creditWaitingDeposits =
    let name = "V1.creditWaitingDeposits"
    in profile name $ do
  State{ accountDBPool = pool } <- ask
  -- guarded, as this runs on the scheduler thread and the process waits on
  -- any of its threads ending: an exception here would take the API down
  -- with it rather than skipping a tick
  ewaiting <- liftIO $ eitherException $ flip runSqlPersistMPool pool
    $ selectList
      [ WalletPaymentDirection ==. Incoming
      , WalletPaymentStatus ==. Settled
      , WalletPaymentCreditedAt ==. Nothing
      ]
      [ Asc WalletPaymentId, LimitTo maxDepositsPerTick ]
  case ewaiting of
    Left err -> runLogging $ $(logError)
      ( "creditWaitingDeposits: could not read the payments waiting to be "
      <> "credited: " <> err
      )
    Right waiting -> mapM_ creditOne waiting
  where
    creditOne (Entity _ payment) = do
      State{ accountDBPool = pool } <- ask
      now <- liftIO getPOSIXTime
      -- guarded per payment, so one, which cannot be credited, is skipped
      -- and the rest of the tick still runs
      ecredited <- liftIO $ eitherException $ flip runSqlPersistMPool pool
        $ creditDepositOnce (walletPaymentPaymentHash payment) now
      case ecredited of
        -- Nothing means somebody else claimed it first, which is not news
        Right (Right _) -> return ()
        Right (Left err) -> runLogging $ $(logError)
          ( "creditWaitingDeposits: a settled payment could not be "
          <> "credited, needs manual reconciliation: " <> describeError err
          )
        Left err -> runLogging $ $(logError)
          ( "creditWaitingDeposits: crediting a settled payment failed, "
          <> "needs manual reconciliation: " <> err
          )
