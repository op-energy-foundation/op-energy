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
  , UnknownOutgoing(..)
  , decideUnknownOutgoing
  , invoiceIsWrittenOff
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

-- | what to do with an outgoing payment, which the wallet says it does not
-- have
data UnknownOutgoing
  = NoteUnknown
    -- ^ the first answer of this kind: record when it came
  | WaitLonger
    -- ^ the wallet has not had it for long enough to give up on it
  | GiveBack
    -- ^ unknown for longer than a payment can still be in flight
  | LeaveForAPerson
    -- ^ the payment was not made at the wallet, which is being asked, or
    -- it is not known which wallet it was made at: what the wallet
    -- answers says nothing about it either way
  deriving (Eq, Show)

-- | decides what to do with an outgoing payment, which the wallet does not
-- have, from the wallet it was made at and how long the answers have been
-- saying so.
--
-- A wallet only knows its own payments. One made at a wallet, which was in
-- use before this one -- another instance, or another wallet of the same
-- instance -- is answered for the same way as one this wallet has lost,
-- and giving it back would return sats, which really did leave the wallet
-- it was made at. Waiting longer does not help: the wallet being asked
-- will never know the payment. So it is left for a person, and so is a
-- payment, whose wallet was never recorded, as there is no telling which
-- of the two it is.
--
-- For a payment of this wallet the sats are still not given back at once:
-- a wallet, which has just been asked to pay and does not say so yet, may
-- be paying, and refunding it then pays it twice. The wait runs from the
-- first answer, which did not have the payment, rather than from when the
-- payment was made: one, which the wallet reported as pending for longer
-- than the grace period and only then lost sight of, would otherwise be
-- given back on the first such answer, while it may still settle.
--
-- Example:
--
-- > decideUnknownOutgoing 3600 now (Just w) w Nothing == NoteUnknown
decideUnknownOutgoing
  :: POSIXTime
     -- ^ how long an unknown payment is left alone
  -> POSIXTime
     -- ^ now
  -> Maybe Text
     -- ^ the wallet the payment was made at, where it was recorded
  -> Text
     -- ^ the wallet, which is being asked
  -> Maybe POSIXTime
     -- ^ when the wallet first did not have it
  -> UnknownOutgoing
decideUnknownOutgoing grace now mmadeAt askedAt munknownSince =
  case mmadeAt of
    Nothing -> LeaveForAPerson
    Just madeAt
      | madeAt /= askedAt -> LeaveForAPerson
      | otherwise -> case munknownSince of
          Nothing -> NoteUnknown
          Just since
            | since + grace < now -> GiveBack
            | otherwise -> WaitLonger

-- | whether an invoice, which the wallet still says nobody has paid, can
-- be written off: its expiry has passed, and the same grace period a lost
-- payment is given has passed on top of it.
--
-- The grace is what keeps a payment, which arrived just before the expiry
-- and is reported after it, from being missed: a written-off invoice is no
-- longer pending, so it is never asked about again and would never be
-- credited. An invoice without an expiry is never written off this way.
--
-- Example:
--
-- > invoiceIsWrittenOff grace now Nothing == False
invoiceIsWrittenOff
  :: POSIXTime
     -- ^ how long after its expiry an invoice is still asked about
  -> POSIXTime
     -- ^ now
  -> Maybe POSIXTime
     -- ^ when the invoice stopped being payable
  -> Bool
invoiceIsWrittenOff grace now =
  maybe False (\expiresAt -> expiresAt + grace < now)

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
      -- the wallet is asked about a hash, while an outgoing row of one hash
      -- exists per account: two accounts may each try to pay the same
      -- invoice, and lightning refusing the second is what normally
      -- separates them. Where more than one of them is still waiting, the
      -- answer names a payment without saying whose, and settling the
      -- wrong row debits an account for a payment it never made. Such a
      -- payment is left for a person to resolve rather than guessed at
      eambiguous <- liftIO $ eitherException $ flip runSqlPersistMPool pool
        $ if walletPaymentDirection payment == Incoming
            then return (0 :: Int)
            else count
              [ WalletPaymentPaymentHash ==. paymentHashV
              , WalletPaymentDirection ==. Outgoing
              , WalletPaymentStatus ==. Pending
              ]
      case eambiguous of
        Left err -> do
          runLogging $ $(logError)
            ( "reconcilePendingPayments: could not count the accounts, "
            <> "which are waiting on the payment "
            <> unPaymentHash paymentHashV <> ": " <> err
            )
          touchNow pool key payment
        Right waitingOnHash | waitingOnHash > 1 -> do
          runLogging $ $(logError)
            ( "reconcilePendingPayments: " <> tshow waitingOnHash
            <> " accounts are waiting on the payment "
            <> unPaymentHash paymentHashV <> ", so what the wallet answers "
            <> "about it does not say whose it is and the payment needs "
            <> "manual reconciliation"
            )
          touchNow pool key payment
        Right _ -> askTheWallet readPayment key payment

    -- asks about one payment, whose wallet answer can be attributed to it
    askTheWallet
      :: MonadIO m
      => (PaymentHash -> IO (Either CallstackError ObservedPayment))
      -> Key WalletPayment
      -> WalletPayment
      -> AppT m ()
    askTheWallet readPayment key payment = do
      State{ accountDBPool = pool } <- ask
      let paymentHashV = walletPaymentPaymentHash payment
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
        Right (Right ObservedPending) -> parkKnown pool key payment
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
        State{ config = config, wallet = walletV } <- ask
        now <- liftIO getPOSIXTime
        let grace =
              fromIntegral (configWalletUnknownPaymentGraceSecs config)
        case decideUnknownOutgoing grace now
               (walletPaymentMadeAtWallet payment)
               (walletIdentity walletV)
               (walletPaymentUnknownSince payment) of
          GiveBack -> do
            runLogging $ $(logError)
              ( "reconcilePendingPayments: the wallet still does not have "
              <> "the outgoing payment "
              <> unPaymentHash (walletPaymentPaymentHash payment)
              <> ", which can no longer be in flight, so what was taken "
              <> "for it is given back"
              )
            recordFailure pool key payment
          NoteUnknown -> do
            runLogging $ $(logError)
              ( "reconcilePendingPayments: the wallet does not have the "
              <> "outgoing payment "
              <> unPaymentHash (walletPaymentPaymentHash payment)
              <> " yet, whose balance is taken, so the wait before it is "
              <> "given back starts now"
              )
            touchWith pool key payment
              [ WalletPaymentUnknownSince =. Just now ] now
          WaitLonger -> do
            runLogging $ $(logError)
              ( "reconcilePendingPayments: the wallet does not have the "
              <> "outgoing payment "
              <> unPaymentHash (walletPaymentPaymentHash payment)
              <> " yet, whose balance is taken"
              )
            park pool key payment
          -- not this wallet's payment to answer for, so nothing it says
          -- is evidence either way and no later answer will be: giving
          -- the sats back would return ones, which really did leave the
          -- wallet it was made at
          LeaveForAPerson -> do
            runLogging $ $(logError)
              ( "reconcilePendingPayments: the outgoing payment "
              <> unPaymentHash (walletPaymentPaymentHash payment)
              <> " was made at "
              <> maybe "a wallet, which was not recorded"
                   (\madeAt-> "the wallet " <> madeAt)
                   (walletPaymentMadeAtWallet payment)
              <> " while this service now runs "
              <> walletIdentity walletV
              <> ", so what that wallet does not have says nothing about "
              <> "it and the payment needs manual reconciliation"
              )
            park pool key payment

    -- a payment the wallet is not finished with
    park
      :: MonadIO m
      => Pool SqlBackend
      -> Key WalletPayment
      -> WalletPayment
      -> AppT m ()
    park pool key payment = parkWith pool key payment []

    -- 'park' for an answer, which did have the payment: an answer from
    -- before, which did not, is no longer the state the payment is in, so
    -- a later one starts its own wait rather than carrying that one on
    parkKnown
      :: MonadIO m
      => Pool SqlBackend
      -> Key WalletPayment
      -> WalletPayment
      -> AppT m ()
    parkKnown pool key payment =
      parkWith pool key payment [ WalletPaymentUnknownSince =. Nothing ]

    -- An invoice, whose expiry and the grace after it have passed, is
    -- written off here: the wallet goes on answering that nobody paid it,
    -- so nothing but the expiry ever ends the asking. Anything else is
    -- only noted as asked about, which sends it to the back of the queue
    -- the next tick reads
    parkWith
      :: MonadIO m
      => Pool SqlBackend
      -> Key WalletPayment
      -> WalletPayment
      -> [Update WalletPayment]
      -> AppT m ()
    parkWith pool key payment extra = do
      State{ config = config } <- ask
      now <- liftIO getPOSIXTime
      let grace =
            fromIntegral (configWalletUnknownPaymentGraceSecs config)
      if walletPaymentDirection payment == Incoming
         && invoiceIsWrittenOff grace now (walletPaymentExpiresAt payment)
        then do
          runLogging $ $(logInfo)
            ( "reconcilePendingPayments: the invoice "
            <> unPaymentHash (walletPaymentPaymentHash payment)
            <> " expired without being paid and has not been paid since, "
            <> "so it is recorded as one, which was never paid"
            )
          finish pool key payment Expired Nothing (Sats 0) "" now
        else touchWith pool key payment extra now

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
    touch pool key payment now = touchWith pool key payment [] now

    -- 'touch', which also records something about the payment in the same
    -- statement, so the note and the place in the queue move together
    touchWith
      :: MonadIO m
      => Pool SqlBackend
      -> Key WalletPayment
      -> WalletPayment
      -> [Update WalletPayment]
      -> POSIXTime
      -> AppT m ()
    touchWith pool key payment extra now = do
      etouched <- liftIO $ eitherException $ flip runSqlPersistMPool pool
        -- the row itself, not its hash: two accounts may each hold a
        -- payment of one hash, and this is the one, which was read.
        -- Matched on when it started as well, as a row, which is taken up
        -- again, is the same row holding a different attempt
        $ updateWhereCount
          [ WalletPaymentId ==. key
          , WalletPaymentCreatedAt ==. walletPaymentCreatedAt payment
          , WalletPaymentStatus ==. Pending
          ]
          ([ WalletPaymentUpdatedAt =. now ] ++ extra)
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
        -- payment, which was read.
        --
        -- When it started pins the attempt as well as the row. A payment,
        -- which failed and was tried again, is the same row with another
        -- amount and another reserve, and this tick is holding what the
        -- attempt before it looked like: without this it would finish the
        -- new attempt and give back the old one's sats
        claimed <- updateWhereCount
          [ WalletPaymentId ==. key
          , WalletPaymentCreatedAt ==. walletPaymentCreatedAt payment
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
