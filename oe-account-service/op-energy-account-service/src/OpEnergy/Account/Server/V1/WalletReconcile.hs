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

import           Control.Exception (throwIO)
import           Control.Monad (when)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Logger (logError, logInfo)
import           Control.Monad.Trans.Reader (ask)
import           Data.Int (Int64)
import           Data.Text (Text)
import qualified Data.Text as Text
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
import           OpEnergy.Error
                 ( CallstackError, describeError, eitherException
                 )

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
        -- oldest asked-about first, rather than lowest id first: a payment
        -- the wallet keeps answering the same way about is asked about
        -- again at the back of the queue, so it can not hold a place in
        -- every tick and leave newer payments never looked at
        $ selectList
          [ WalletPaymentStatus ==. Pending ]
          [ Asc WalletPaymentUpdatedAt, LimitTo maxPaymentsPerTick ]
      case ewaiting of
        Left err -> runLogging $ $(logError)
          ( "reconcilePendingPayments: could not read the payments waiting "
          <> "to be asked about: " <> err
          )
        Right waiting -> mapM_ (askAbout readPayment) waiting
  where
    askAbout
      :: MonadIO m
      => (PaymentHash -> IO (Either CallstackError ObservedPayment))
      -> Entity WalletPayment
      -> AppT m ()
    askAbout readPayment (Entity _ payment) = do
      State{ accountDBPool = pool } <- ask
      let paymentHashV = walletPaymentPaymentHash payment
      -- guarded per payment, so one the wallet can not answer for is
      -- skipped and the rest of the tick still runs
      eobserved <- liftIO $ eitherException $ readPayment paymentHashV
      case eobserved of
        Left err -> do
          runLogging $ $(logError)
            ( "reconcilePendingPayments: asking about the payment "
            <> unPaymentHash paymentHashV <> " failed: " <> err
            )
          park pool payment
        Right (Left err) -> do
          runLogging $ $(logError)
            ( "reconcilePendingPayments: the wallet could not answer about "
            <> "the payment " <> unPaymentHash paymentHashV <> ": "
            <> describeError err
            )
          park pool payment
        -- not finished yet, which is the usual answer: asked again later
        Right (Right ObservedPending) -> park pool payment
        Right (Right (ObservedSettled fee)) -> settle pool payment fee
        Right (Right ObservedFailed) -> recordFailure pool payment
        Right (Right ObservedUnknown) -> forget pool payment

    -- a payment, which has moved its sats
    settle
      :: MonadIO m
      => Pool SqlBackend
      -> WalletPayment
      -> Sats
      -> AppT m ()
    settle pool payment fee = do
      now <- liftIO getPOSIXTime
      let reserve = reserveOf payment
      -- a fee above the reserve is recorded rather than taken: the sats
      -- are already gone and the account was told what it would pay. It is
      -- asked about rather than subtracted, as Sats counts upwards only
      -- and the difference would otherwise wrap into an enormous credit
      when (fee > reserve) $ runLogging $ $(logError)
        ( "reconcilePendingPayments: the payment "
        <> unPaymentHash (walletPaymentPaymentHash payment) <> " cost "
        <> tshow (unSats fee) <> " sats while " <> tshow (unSats reserve)
        <> " sats were reserved for it, so the wallet is short by the "
        <> "difference and needs manual reconciliation"
        )
      -- an outgoing payment is finished here: the amount is gone for good
      -- and the reserve, which the fee did not use, goes back. An incoming
      -- one is credited by creditWaitingDeposits instead
      finish pool payment Settled (Just fee)
        (if fee > reserve then Sats 0 else reserve - fee)
        "the fee reserve, which the payment did not use" now

    -- a payment the wallet says it did not make
    recordFailure
      :: MonadIO m
      => Pool SqlBackend
      -> WalletPayment
      -> AppT m ()
    recordFailure pool payment = do
      now <- liftIO getPOSIXTime
      -- nothing left the wallet, so everything, which was taken for it,
      -- goes back. An incoming payment took nothing and gets nothing back
      finish pool payment Failed Nothing
        (walletPaymentAmountSats payment + reserveOf payment)
        "a payment, which the wallet did not make" now

    -- a payment the wallet does not have at all
    forget
      :: MonadIO m
      => Pool SqlBackend
      -> WalletPayment
      -> AppT m ()
    forget pool payment = case walletPaymentDirection payment of
      -- an invoice of another wallet -- every invoice of a backend used
      -- before this one reads this way. It is written off once it can no
      -- longer be paid, and until then it is asked about again, as a
      -- wallet, which answers this way about an invoice it does have,
      -- would otherwise have it abandoned
      Incoming -> park pool payment
      -- the balance was taken for this payment. It is not given back
      -- here: a wallet, which does not have a payment it did make,
      -- would have it refunded while its sats are gone, which is worse
      -- than leaving it to be looked at
      Outgoing -> do
        runLogging $ $(logError)
          ( "reconcilePendingPayments: the wallet does not have the "
          <> "outgoing payment "
          <> unPaymentHash (walletPaymentPaymentHash payment)
          <> ", whose balance is taken, so it needs manual reconciliation"
          )
        park pool payment

    -- a payment the wallet is not finished with. An invoice, whose expiry
    -- has passed, is written off here: the wallet goes on answering that
    -- nobody paid it, so nothing but the expiry ever ends the asking.
    -- Anything else is only noted as asked about, which sends it to the
    -- back of the queue the next tick reads
    park
      :: MonadIO m
      => Pool SqlBackend
      -> WalletPayment
      -> AppT m ()
    park pool payment = do
      now <- liftIO getPOSIXTime
      if walletPaymentDirection payment == Incoming
         && maybe False (< now) (walletPaymentExpiresAt payment)
        then do
          runLogging $ $(logInfo)
            ( "reconcilePendingPayments: the invoice "
            <> unPaymentHash (walletPaymentPaymentHash payment)
            <> " has expired without being paid, so it is recorded as one, "
            <> "which was never paid"
            )
          finish pool payment Expired Nothing (Sats 0) "" now
        else touch pool payment now

    -- records that the payment was asked about, without changing what it
    -- is. Only the order the next tick reads the payments in depends on it
    touch
      :: MonadIO m
      => Pool SqlBackend
      -> WalletPayment
      -> POSIXTime
      -> AppT m ()
    touch pool payment now = do
      etouched <- liftIO $ eitherException $ flip runSqlPersistMPool pool
        $ updateWhereCount
          [ WalletPaymentPaymentHash ==. walletPaymentPaymentHash payment
          , WalletPaymentStatus ==. Pending
          ]
          [ WalletPaymentUpdatedAt =. now ]
      case etouched of
        Right (_ :: Int64) -> return ()
        Left err -> runLogging $ $(logError)
          ( "reconcilePendingPayments: could not record that the payment "
          <> unPaymentHash (walletPaymentPaymentHash payment)
          <> " was asked about: " <> err
          )

    -- the sats, which were taken on top of the amount, to cover a fee
    -- not yet known. They are kept in the fee column until the real fee
    -- replaces them
    reserveOf :: WalletPayment -> Sats
    reserveOf payment = walletPaymentFeeSats payment

    -- takes the payment out of Pending and gives back the sats, which are
    -- no longer needed for it, in one transaction: a payment, which is out
    -- of Pending, has always had its balance moved with it. Only the
    -- caller, which took it out of Pending, moves anything, so two ticks
    -- arriving together give the same sats back once.
    --
    -- Only an outgoing payment gives anything back: an incoming one took
    -- no balance, and creditWaitingDeposits credits it from the status
    -- written here
    finish
      :: MonadIO m
      => Pool SqlBackend
      -> WalletPayment
      -> WalletPaymentStatus
      -> Maybe Sats
      -> Sats
      -> Text
      -> POSIXTime
      -> AppT m ()
    finish pool payment status mfee back reason now = do
      let paymentHashV = walletPaymentPaymentHash payment
          givesBack = walletPaymentDirection payment == Outgoing
                   && back > Sats 0
      edone <- liftIO $ eitherException $ flip runSqlPersistMPool pool $ do
        claimed <- updateWhereCount
          [ WalletPaymentPaymentHash ==. paymentHashV
          , WalletPaymentStatus ==. Pending
          ]
          ( [ WalletPaymentStatus =. status
            , WalletPaymentUpdatedAt =. now
            ]
         ++ maybe [] (\fee-> [ WalletPaymentFeeSats =. fee ]) mfee
          )
        when ((claimed :: Int64) == 1 && givesBack) $ do
          egiven <- adjustBalanceTx (walletPaymentPersonId payment) Credit
            back Refund (Just reason) now
          -- thrown rather than returned, so the claim above is rolled back
          -- with it: a payment, whose sats could not be given back, stays
          -- Pending and is tried again, rather than being left finished
          -- with the account short of them
          either
            (liftIO . throwIO . userError . Text.unpack . describeError)
            (const (return ()))
            egiven
      case edone of
        Right () -> return ()
        Left err -> runLogging $ $(logError)
          ( "reconcilePendingPayments: the payment "
          <> unPaymentHash paymentHashV <> " is "
          <> walletPaymentStatusToText status
          <> " at the wallet and could not be recorded as such, so it is "
          <> "left pending to be tried again: " <> err
          )
