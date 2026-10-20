{-- | Asking the wallet what became of the payments, which are still
 - waiting.
 -
 - A wallet with a node of its own is not able to tell this service what
 - happened: the sats move at the node, and nothing reaches this process.
 - So the scheduler asks, payment by payment, and records what it is told.
 -
 - For an incoming payment that is all this does: marking it settled is
 - enough, and
 - 'OpEnergy.Account.Server.V1.WalletDeposits.creditWaitingDeposits'
 - credits it exactly once from that.
 -
 - An outgoing payment is finished here instead, as nothing else is waiting
 - for it: the balance was taken when it was made, together with a reserve
 - for a fee, which was not known yet. A payment, which went, gives back
 - what the fee did not use; a payment, which the wallet says failed, gives
 - back everything it took. Each of those happens only for the tick, which
 - claims the payment out of 'Pending', so two ticks can not give the same
 - sats back twice.
 -
 - A backend, which settles its own payments, answers 'Nothing' to
 - 'walletReadPayment' and this does nothing at all for it.
 -}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
module OpEnergy.Account.Server.V1.WalletReconcile
  ( reconcilePendingPayments
  , maxPaymentsPerTick
  ) where

import           Control.Monad (when)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Logger (logError, logInfo)
import           Control.Monad.Trans.Reader (ask)
import           Data.Int (Int64)
import           Data.Text (Text)
import           Data.Text.Show (tshow)
import           Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import           Database.Persist
import           Database.Persist.Postgresql (runSqlPersistMPool)
import           Database.Persist.Sql (SqlBackend, updateWhereCount)
import           Data.Pool (Pool)
import           Prometheus (MonadMonitor)

import           Data.OpEnergy.Account.API.V1.Sats (Sats(..))
import           Data.OpEnergy.Account.API.V2.LedgerReason (LedgerReason(..))
import           Data.OpEnergy.Account.API.V2.PaymentHash
                 ( PaymentHash, unPaymentHash
                 )

import           OpEnergy.Account.Server.V1.Class
                 ( AppT, State(..), profile, runLogging
                 )
import           OpEnergy.Account.Server.V1.LedgerEntry
                 ( LedgerDirection(..), adjustBalanceTx
                 )
import           OpEnergy.Account.Server.V1.Wallet.Class
                 ( ObservedPayment(..), WalletBackend(..)
                 )
import           OpEnergy.Account.Server.V1.WalletPayment
import           OpEnergy.Error (describeError, eitherException)

-- | how many waiting payments one tick asks about. Each one is a call to
-- the wallet, so this bounds both the time a tick takes and the requests
-- the wallet is sent
maxPaymentsPerTick :: Int
maxPaymentsPerTick = 50

-- | asks the wallet about the payments, which are still pending, and
-- records the answers. Run from the scheduler.
--
-- Only a payment, which is still pending, is changed, and only into the
-- state the wallet reports: an answer this service does not understand
-- leaves the payment alone, to be asked about again next tick, rather than
-- finishing it on a guess
reconcilePendingPayments :: (MonadIO m, MonadMonitor m) => AppT m ()
reconcilePendingPayments =
    let name = "V1.reconcilePendingPayments"
    in profile name $ do
  State{ accountDBPool = pool, wallet = walletV } <- ask
  case walletReadPayment walletV of
    -- a backend, which settles its own payments, has nothing to be asked
    Nothing -> return ()
    Just readPayment -> do
      -- guarded, as this runs on the scheduler thread and the process
      -- waits on any of its threads ending: an exception here would take
      -- the API down with it rather than skipping a tick
      ewaiting <- liftIO $ eitherException $ flip runSqlPersistMPool pool
        $ selectList
          [ WalletPaymentStatus ==. Pending ]
          [ Asc WalletPaymentId, LimitTo maxPaymentsPerTick ]
      case ewaiting of
        Left err -> runLogging $ $(logError)
          ( "reconcilePendingPayments: could not read the payments waiting "
          <> "to be asked about: " <> err
          )
        Right waiting -> mapM_ (askAbout readPayment) waiting
  where
    askAbout readPayment (Entity _ payment) = do
      State{ accountDBPool = pool } <- ask
      let paymentHashV = walletPaymentPaymentHash payment
      -- guarded per payment, so one the wallet can not answer for is
      -- skipped and the rest of the tick still runs
      eobserved <- liftIO $ eitherException $ readPayment paymentHashV
      case eobserved of
        Left err -> runLogging $ $(logError)
          ( "reconcilePendingPayments: asking about the payment "
          <> unPaymentHash paymentHashV <> " failed: " <> err
          )
        Right (Left err) -> runLogging $ $(logError)
          ( "reconcilePendingPayments: the wallet could not answer about "
          <> "the payment " <> unPaymentHash paymentHashV <> ": "
          <> describeError err
          )
        -- not finished yet, which is the usual answer: asked again next tick
        Right (Right ObservedPending) -> return ()
        Right (Right (ObservedSettled fee)) -> settle pool payment fee
        Right (Right ObservedFailed) -> recordFailure pool payment
        Right (Right ObservedUnknown) -> forget pool payment

    -- | a payment, which has moved its sats
    settle pool payment fee = do
      now <- liftIO getPOSIXTime
      let paymentHashV = walletPaymentPaymentHash payment
      claimed <- claim pool paymentHashV Settled (Just fee) now
      -- an outgoing payment is finished here: the amount is gone for good
      -- and the reserve, which the fee did not use, goes back. An incoming
      -- one is credited by creditWaitingDeposits instead
      when (claimed && walletPaymentDirection payment == Outgoing)
        $ giveBack pool payment (reserveOf payment - fee)
            "the fee reserve, which the payment did not use" now

    -- | a payment the wallet says it did not make
    recordFailure pool payment = do
      now <- liftIO getPOSIXTime
      let paymentHashV = walletPaymentPaymentHash payment
      claimed <- claim pool paymentHashV Failed Nothing now
      -- nothing left the wallet, so everything, which was taken for it,
      -- goes back. An incoming payment took nothing and gets nothing back
      when (claimed && walletPaymentDirection payment == Outgoing)
        $ giveBack pool payment
            (walletPaymentAmountSats payment + reserveOf payment)
            "a payment, which the wallet did not make" now

    -- | a payment the wallet does not have at all
    forget pool payment = do
      now <- liftIO getPOSIXTime
      let paymentHashV = walletPaymentPaymentHash payment
      case walletPaymentDirection payment of
        -- an invoice of another wallet -- every invoice of a backend used
        -- before this one reads this way. It is written off once it can no
        -- longer be paid, and until then it is asked about again, as a
        -- wallet, which answers this way about an invoice it does have,
        -- would otherwise have it abandoned
        Incoming
          | maybe False (< now) (walletPaymentExpiresAt payment) -> do
              runLogging $ $(logInfo)
                ( "reconcilePendingPayments: the expired invoice "
                <> unPaymentHash paymentHashV <> " is not the wallet's, so "
                <> "it is recorded as one, which was never paid"
                )
              _ <- claim pool paymentHashV Expired Nothing now
              return ()
          | otherwise -> runLogging $ $(logInfo)
              ( "reconcilePendingPayments: the wallet does not have the "
              <> "invoice " <> unPaymentHash paymentHashV
              <> ", which has not expired yet"
              )
        -- the balance was taken for this payment. It is not given back
        -- here: a wallet, which does not have a payment it did make,
        -- would have it refunded while its sats are gone, which is worse
        -- than leaving it to be looked at
        Outgoing -> runLogging $ $(logError)
          ( "reconcilePendingPayments: the wallet does not have the "
          <> "outgoing payment " <> unPaymentHash paymentHashV
          <> ", whose balance is taken, so it needs manual reconciliation"
          )

    -- | the sats, which were taken on top of the amount, to cover a fee
    -- not yet known. They are kept in the fee column until the real fee
    -- replaces them
    reserveOf payment = walletPaymentFeeSats payment

    -- | takes the payment out of Pending. Answers whether this caller is
    -- the one, which took it: only that caller moves a balance for it, so
    -- two ticks arriving together give the same sats back once
    claim
      :: MonadIO m
      => Pool SqlBackend
      -> PaymentHash
      -> WalletPaymentStatus
      -> Maybe Sats
      -> POSIXTime
      -> AppT m Bool
    claim pool paymentHashV status mfee now = do
      eclaimed <- liftIO $ eitherException $ flip runSqlPersistMPool pool
        $ updateWhereCount
          [ WalletPaymentPaymentHash ==. paymentHashV
          , WalletPaymentStatus ==. Pending
          ]
          ( [ WalletPaymentStatus =. status
            , WalletPaymentUpdatedAt =. now
            ]
         ++ maybe [] (\fee-> [ WalletPaymentFeeSats =. fee ]) mfee
          )
      case eclaimed of
        Right (claimed :: Int64) -> return $! claimed == 1
        Left err -> do
          runLogging $ $(logError)
            ( "reconcilePendingPayments: the payment "
            <> unPaymentHash paymentHashV <> " is "
            <> walletPaymentStatusToText status
            <> " at the wallet and could not be recorded as such, so it "
            <> "needs manual reconciliation: " <> err
            )
          return False

    -- | returns sats, which were taken for a payment and are not needed
    giveBack
      :: MonadIO m
      => Pool SqlBackend
      -> WalletPayment
      -> Sats
      -> Text
      -> POSIXTime
      -> AppT m ()
    giveBack pool payment amount reason now = when (amount > Sats 0) $ do
      egiven <- liftIO $ eitherException $ flip runSqlPersistMPool pool
        $ adjustBalanceTx (walletPaymentPersonId payment) Credit amount
            Refund (Just reason) now
      case egiven of
        Right (Right _) -> return ()
        Right (Left err) -> logFailedGiveBack payment amount
          (describeError err)
        Left err -> logFailedGiveBack payment amount err

    logFailedGiveBack payment amount reason = runLogging $ $(logError)
      ( "reconcilePendingPayments: giving back " <> tshow amount
      <> " sats of the payment "
      <> unPaymentHash (walletPaymentPaymentHash payment)
      <> " failed, so the account is short by them and needs manual "
      <> "reconciliation: " <> reason
      )
