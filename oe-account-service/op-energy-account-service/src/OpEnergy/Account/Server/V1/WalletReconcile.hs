{-- | Asking the wallet what became of the payments, which are still
 - waiting.
 -
 - A wallet with a node of its own is not able to tell this service what
 - happened: the sats move at the node, and nothing reaches this process.
 - So the scheduler asks, payment by payment, and records what it is told.
 -
 - For an incoming payment this marks it settled, which is enough for
 - 'OpEnergy.Account.Server.V1.WalletDeposits.creditWaitingDeposits' to
 - credit it exactly once, or writes it off as expired once its expiry has
 - passed and the wallet still reports nobody has paid it.
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
import           OpEnergy.Account.Server.V1.Config (Config(..))
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
-- Only a payment, which is still pending, is changed, and only on an
-- answer: an answer this service does not understand, and a call, which
-- failed, leave the payment alone to be asked about again rather than
-- finishing it on a guess.
--
-- The one state, which is written without the wallet reporting it, is
-- 'Expired', for an invoice, whose expiry has passed and which the wallet
-- still says nobody has paid: the wallet goes on answering that way for
-- good, so nothing else would ever end the asking
reconcilePendingPayments :: (MonadIO m, MonadMonitor m) => AppT m ()
reconcilePendingPayments =
    let name = "V1.reconcilePendingPayments"
    in profile name $ do
  State{ accountDBPool = pool, config = config, wallet = walletV } <- ask
  now <- liftIO getPOSIXTime
  let notAskedSince = now
        - fromIntegral (configWalletReconcileIntervalSecs config)
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
        -- every tick and leave newer payments never looked at.
        --
        -- One, which was asked about recently, is left out: every ask is
        -- a call to the wallet, and a payment does not finish quickly
        -- enough to be worth asking about on every tick
        $ selectList
          [ WalletPaymentStatus ==. Pending
          , WalletPaymentUpdatedAt <=. notAskedSince
          ]
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
    askAbout readPayment (Entity key payment) = do
      State{ accountDBPool = pool } <- ask
      let paymentHashV = walletPaymentPaymentHash payment
      -- guarded per payment, so one the wallet can not answer for is
      -- skipped and the rest of the tick still runs
      eobserved <- liftIO $ eitherException $ readPayment paymentHashV
      case eobserved of
        -- a call, which failed, says nothing about the payment: it is
        -- only noted as asked about, never written off. The wallet may
        -- have been paid while it was unreachable
        Left err -> do
          runLogging $ $(logError)
            ( "reconcilePendingPayments: asking about the payment "
            <> unPaymentHash paymentHashV <> " failed: " <> err
            )
          touchNow pool key payment
        Right (Left err) -> do
          runLogging $ $(logError)
            ( "reconcilePendingPayments: the wallet could not answer about "
            <> "the payment " <> unPaymentHash paymentHashV <> ": "
            <> describeError err
            )
          touchNow pool key payment
        -- not finished yet, which is the usual answer: asked again later
        Right (Right ObservedPending) -> park pool key payment
        Right (Right (ObservedSettled fee)) -> settle pool key payment fee
        Right (Right ObservedFailed) -> recordFailure pool key payment
        Right (Right ObservedUnknown) -> forget pool key payment

    -- a payment, which has moved its sats
    settle
      :: MonadIO m
      => Pool SqlBackend
      -> Key WalletPayment
      -> WalletPayment
      -> Sats
      -> AppT m ()
    settle pool key payment fee = do
      now <- liftIO getPOSIXTime
      let reserve = reserveOf payment
      -- a fee above the reserve is recorded rather than taken: the sats
      -- are already gone and the account was told what it would pay. It is
      -- asked about rather than subtracted, as Sats counts upwards only
      -- and the difference would otherwise wrap into an enormous credit
      -- outgoing only: nothing was sent for a payment, which arrived, so
      -- there is no reserve it could have overrun
      when (fee > reserve && walletPaymentDirection payment == Outgoing)
        $ runLogging $ $(logError)
        ( "reconcilePendingPayments: the payment "
        <> unPaymentHash (walletPaymentPaymentHash payment) <> " cost "
        <> tshow (unSats fee) <> " sats while " <> tshow (unSats reserve)
        <> " sats were reserved for it, so the wallet is short by the "
        <> "difference and needs manual reconciliation"
        )
      -- an outgoing payment is finished here: the amount is gone for good
      -- and the reserve, which the fee did not use, goes back. An incoming
      -- one is credited by creditWaitingDeposits instead, for the amount
      -- of its invoice, so a fee reported against it is not recorded: the
      -- column holds what a payment cost this service to send, and
      -- nothing was sent for one, which arrived
      finish pool key payment Settled
        (case walletPaymentDirection payment of
           Outgoing -> Just fee
           Incoming -> Nothing
        )
        (if fee > reserve then Sats 0 else reserve - fee)
        "the fee reserve, which the payment did not use" now

    -- a payment the wallet says it did not make
    recordFailure
      :: MonadIO m
      => Pool SqlBackend
      -> Key WalletPayment
      -> WalletPayment
      -> AppT m ()
    recordFailure pool key payment = do
      now <- liftIO getPOSIXTime
      -- nothing left the wallet, so everything, which was taken for it,
      -- goes back. An incoming payment took nothing and gets nothing back
      finish pool key payment Failed Nothing
        (walletPaymentAmountSats payment + reserveOf payment)
        "a payment, which the wallet did not make" now

    -- a payment the wallet does not have at all
    forget
      :: MonadIO m
      => Pool SqlBackend
      -> Key WalletPayment
      -> WalletPayment
      -> AppT m ()
    forget pool key payment = case walletPaymentDirection payment of
      -- an invoice of another wallet -- every invoice of a backend used
      -- before this one reads this way. It is written off once it can no
      -- longer be paid, and until then it is asked about again, as a
      -- wallet, which answers this way about an invoice it does have,
      -- would otherwise have it abandoned
      Incoming -> park pool key payment
      -- the balance was taken for this payment, so it is not given back
      -- at once: a wallet, which has just been asked to pay and does not
      -- say so yet, may be paying, and refunding it then pays it twice.
      --
      -- It is given back once the payment is older than any payment can
      -- still be in flight. A wallet, which was unreachable when the
      -- payment was sent, never received it and will never have it, so
      -- the sats would otherwise be held for good
      Outgoing -> do
        State{ config = config } <- ask
        now <- liftIO getPOSIXTime
        let grace =
              fromIntegral (configWalletUnknownPaymentGraceSecs config)
        if walletPaymentCreatedAt payment + grace < now
          then do
            runLogging $ $(logError)
              ( "reconcilePendingPayments: the wallet still does not have "
              <> "the outgoing payment "
              <> unPaymentHash (walletPaymentPaymentHash payment)
              <> ", which can no longer be in flight, so what was taken "
              <> "for it is given back"
              )
            recordFailure pool key payment
          else do
            runLogging $ $(logError)
              ( "reconcilePendingPayments: the wallet does not have the "
              <> "outgoing payment "
              <> unPaymentHash (walletPaymentPaymentHash payment)
              <> " yet, whose balance is taken"
              )
            park pool key payment

    -- a payment the wallet is not finished with. An invoice, whose expiry
    -- has passed, is written off here: the wallet goes on answering that
    -- nobody paid it, so nothing but the expiry ever ends the asking.
    -- Anything else is only noted as asked about, which sends it to the
    -- back of the queue the next tick reads
    park
      :: MonadIO m
      => Pool SqlBackend
      -> Key WalletPayment
      -> WalletPayment
      -> AppT m ()
    park pool key payment = do
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
          finish pool key payment Expired Nothing (Sats 0) "" now
        else touch pool key payment now

    -- 'touch' for a caller, which has not read the clock
    touchNow
      :: MonadIO m
      => Pool SqlBackend
      -> Key WalletPayment
      -> WalletPayment
      -> AppT m ()
    touchNow pool key payment =
      liftIO getPOSIXTime >>= touch pool key payment

    -- records that the payment was asked about, without changing what it
    -- is. Only the order the next tick reads the payments in depends on it
    touch
      :: MonadIO m
      => Pool SqlBackend
      -> Key WalletPayment
      -> WalletPayment
      -> POSIXTime
      -> AppT m ()
    touch pool key payment now = do
      etouched <- liftIO $ eitherException $ flip runSqlPersistMPool pool
        -- the row itself, not its hash: two accounts may each hold a
        -- payment of one hash, and this is the one, which was read
        $ updateWhereCount
          [ WalletPaymentId ==. key
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
      -> Key WalletPayment
      -> WalletPayment
      -> WalletPaymentStatus
      -> Maybe Sats
      -> Sats
      -> Text
      -> POSIXTime
      -> AppT m ()
    finish pool key payment status mfee back reason now = do
      let paymentHashV = walletPaymentPaymentHash payment
          givesBack = walletPaymentDirection payment == Outgoing
                   && back > Sats 0
      edone <- liftIO $ eitherException $ flip runSqlPersistMPool pool $ do
        -- the row itself: a hash names a row each way round, and an
        -- outgoing one of it per account, so only its id finishes the
        -- payment, which was read
        claimed <- updateWhereCount
          [ WalletPaymentId ==. key
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
