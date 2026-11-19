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
import           Data.Maybe (fromMaybe, isJust)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Text.Show (tshow)
import           Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import           Database.Persist
import           Database.Persist.Sql (updateWhereCount)
import           Prometheus (MonadMonitor)

import           Data.OpEnergy.Account.API.V1.Sats (Sats(..))
import           Data.OpEnergy.Account.API.V2.LedgerReason (LedgerReason(..))
import           Data.OpEnergy.Account.API.V2.PaymentHash
                 ( unPaymentHash
                 )

import           OpEnergy.Account.Server.V1.Class
                 ( AppT, State(..), profile, runLogging, withDBTransaction
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
-- | whether the wallet being asked is the wallet the payment was made at.
--
-- A payment belongs to the wallet, which made it: another wallet has no
-- record of it, so what that one answers describes nothing. A row without
-- a wallet recorded is from before this was written down and is not
-- claimed either way.
--
-- Example:
--
-- > answeredByOriginatingWallet payment wallet
answeredByOriginatingWallet :: WalletPayment -> WalletBackend -> Bool
answeredByOriginatingWallet payment walletV =
  case walletPaymentMadeAtWallet payment of
    Nothing -> False
    Just madeAt -> madeAt == walletIdentity walletV

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
  State{ config = config, wallet = walletV } <- ask
  now <- liftIO getPOSIXTime
  -- never before the epoch: the moment is stored as a Word32, so a
  -- negative one is written as an enormous one -- a cutoff in the future,
  -- which every pending payment is older than, and the interval would be
  -- ignored on every tick. An interval, which outruns the clock, simply
  -- means nothing has been asked about yet
  let notAskedSince = max 0
        ( now - fromIntegral (configWalletReconcileIntervalSecs config) )
  case walletReadPayment walletV of
    -- a backend, which settles its own payments, has nothing to be asked
    Nothing -> return ()
    Just readPayment -> do
      -- withDBTransaction, as this runs on the scheduler thread and the
      -- process waits on any of its threads ending: an exception here
      -- would take the API down with it rather than skipping a tick. It
      -- logs the one it catches and answers Nothing
      mwaiting <- withDBTransaction ""
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
      case mwaiting of
        Nothing -> return ()
        Just waiting -> mapM_ (askAbout readPayment) waiting
  where
    -- takes the payment before the wallet is called, so only one tick
    -- asks about it.
    --
    -- Reading the payments, which are due, and asking about them are not
    -- one step: two instances sharing this database read the same rows in
    -- between and would both call the wallet about every one of them,
    -- asking twice as often as the interval allows. The moment a payment
    -- was last asked about is advanced here, and only where it is still
    -- the moment, which was read -- so of two ticks, which read the same
    -- row, one takes it and the other finds it already taken.
    --
    -- The call to the wallet is made after this transaction has closed,
    -- so nothing is held open while the wallet answers.
    askAbout
      :: (MonadIO m, MonadMonitor m)
      => (Text -> IO (Either CallstackError ObservedPayment))
      -> Entity WalletPayment
      -> AppT m ()
    askAbout readPayment (Entity key payment) = do
      now <- liftIO getPOSIXTime
      mclaimed <- withDBTransaction "" $ updateWhereCount
        [ WalletPaymentId ==. key
        , WalletPaymentUpdatedAt ==. walletPaymentUpdatedAt payment
        ]
        [ WalletPaymentUpdatedAt =. now ]
      case mclaimed of
        -- taken by this tick, and carried on as the row this tick wrote
        -- rather than the one it read: the moment above is what every
        -- write of this tick matches on, so a tick, whose call to the
        -- wallet outlasted the interval and whose row another tick has
        -- taken up since, writes nothing on the strength of an answer
        -- about the attempt it was holding
        Just (1 :: Int64) -> askClaimed readPayment
          (Entity key payment { walletPaymentUpdatedAt = now })
        -- another tick has it, or the row moved on since it was read.
        -- A read, which failed, is left alone as well: the tick, which
        -- could not claim it, is not the tick to call the wallet for it
        _ -> return ()

    askClaimed
      :: (MonadIO m, MonadMonitor m)
      => (Text -> IO (Either CallstackError ObservedPayment))
      -> Entity WalletPayment
      -> AppT m ()
    askClaimed readPayment (Entity key payment) = do
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
      -- the hash is only ambiguous where the hash is what the wallet
      -- will be asked about. An attempt, whose own name was recorded, is
      -- asked about by that name and answered for on its own, so no other
      -- payment of the same hash can be mistaken for it -- and counting
      -- them would leave an internal transfer, whose two rows always
      -- share a hash, ambiguous for good and its reserve held
      mambiguous <- withDBTransaction ""
        $ if walletPaymentDirection payment == Incoming
             || isJust (walletPaymentCheckingId payment)
            then return (0 :: Int)
            -- a row, which has settled, counts as well as one, which is
            -- waiting: where one account has already paid the invoice and
            -- another's attempt never reached the wallet, the second is
            -- the only row still waiting, and the wallet -- asked about
            -- the hash -- answers with the payment the first account
            -- made. Counting only the waiting rows would leave that one
            -- unambiguous and debit the second account for the first
            -- account's payment
            -- direction is not named: the wallet is asked about a hash
            -- and answers about a payment, without saying which way it
            -- went, so an invoice of this service, which somebody else
            -- paid, answers for an outgoing attempt of the same hash as
            -- readily as that attempt would. Counting the incoming rows
            -- as well leaves such a hash ambiguous rather than settling
            -- the attempt on a payment, which was never its own
            else count
              [ WalletPaymentPaymentHash ==. paymentHashV
              , WalletPaymentStatus <-. [ Pending, Settled ]
              ]
      case mambiguous of
        -- withDBTransaction has logged what went wrong
        Nothing -> do
          runLogging $ $(logError)
            ( "reconcilePendingPayments: could not count the accounts, "
            <> "which are waiting on the payment "
            <> unPaymentHash paymentHashV
            )
          touchNow key payment
        Just waitingOnHash | waitingOnHash > 1 -> do
          runLogging $ $(logError)
            ( "reconcilePendingPayments: " <> tshow waitingOnHash
            <> " accounts are waiting on the payment "
            <> unPaymentHash paymentHashV <> ", so what the wallet answers "
            <> "about it does not say whose it is and the payment needs "
            <> "manual reconciliation"
            )
          touchNow key payment
        Just _ -> askTheWallet readPayment key payment

    -- asks about one payment, whose wallet answer can be attributed to it
    askTheWallet
      :: (MonadIO m, MonadMonitor m)
      => (Text -> IO (Either CallstackError ObservedPayment))
      -> Key WalletPayment
      -> WalletPayment
      -> AppT m ()
    askTheWallet readPayment key payment = do
      State{ wallet = walletV } <- ask
      let paymentHashV = walletPaymentPaymentHash payment
      -- asked about by the name the wallet gave this attempt where there
      -- is one, and by the hash otherwise. A hash names the invoice, so
      -- an invoice of this service, which somebody else paid, and another
      -- account's attempt at the same invoice both answer to it; the name
      -- answers for this attempt alone. A row, which was written before
      -- this, and one the wallet never answered for have no name, so they
      -- are asked about as they were
      eobserved <- liftIO $ eitherException $ readPayment
        $ fromMaybe (unPaymentHash paymentHashV)
            (walletPaymentCheckingId payment)
      case eobserved of
        -- a call, which failed, says nothing about the payment: it is
        -- only noted as asked about, never written off. The wallet may
        -- have been paid while it was unreachable
        Left err -> do
          runLogging $ $(logError)
            ( "reconcilePendingPayments: asking about the payment "
            <> unPaymentHash paymentHashV <> " failed: " <> err
            )
          touchNow key payment
        Right (Left err) -> do
          runLogging $ $(logError)
            ( "reconcilePendingPayments: the wallet could not answer about "
            <> "the payment " <> unPaymentHash paymentHashV <> ": "
            <> describeError err
            )
          touchNow key payment
        -- an answer about an outgoing payment is only acted on where it
        -- came from the wallet the payment was made at. A replacement
        -- wallet knows nothing of what the one before it paid, so its
        -- "failed" would give back the sats of a payment, which went, and
        -- its "settled" would finish one it never made. Only the wallet,
        -- which has the payment, can say what became of it; anything else
        -- is parked for a person. Incoming rows are not checked this way:
        -- an invoice is settled by whoever pays it
        Right (Right _observed)
          | walletPaymentDirection payment == Outgoing
              && not (answeredByOriginatingWallet payment walletV) -> do
              runLogging $ $(logError)
                ( "reconcilePendingPayments: the outgoing payment "
                <> unPaymentHash paymentHashV <> " was made at "
                <> maybe "a wallet, which was not recorded"
                     (\madeAt-> "the wallet " <> madeAt)
                     (walletPaymentMadeAtWallet payment)
                <> " while this service now runs " <> walletIdentity walletV
                <> ", so what this wallet answers about it says nothing "
                <> "and it needs manual reconciliation"
                )
              park key payment
        -- not finished yet, which is the usual answer: asked again later
        Right (Right ObservedPending) -> parkKnown key payment
        Right (Right (ObservedSettled fee)) -> settle key payment fee
        Right (Right ObservedFailed) -> recordFailure key payment
        Right (Right ObservedUnknown) -> forget key payment

    -- a payment, which has moved its sats
    settle
      :: (MonadIO m, MonadMonitor m)
      => Key WalletPayment
      -> WalletPayment
      -> Sats
      -> AppT m ()
    settle key payment fee = do
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
      finish key payment Settled
        (case walletPaymentDirection payment of
           Outgoing -> Just fee
           Incoming -> Nothing
        )
        (if fee > reserve then Sats 0 else reserve - fee)
        "the fee reserve, which the payment did not use" now

    -- a payment the wallet says it did not make
    recordFailure
      :: (MonadIO m, MonadMonitor m)
      => Key WalletPayment
      -> WalletPayment
      -> AppT m ()
    recordFailure key payment = do
      now <- liftIO getPOSIXTime
      case walletPaymentDirection payment of
        -- an invoice is never written off on a failure answer, whichever
        -- wallet issued it. An incoming row carries no name for an
        -- attempt, so the wallet is asked about it by hash, and a hash
        -- names an invoice rather than one payment of it: an outgoing
        -- attempt at this very invoice, by this service or by the account
        -- which owes it, answers to the same hash and reports its own
        -- failure. Taken as the invoice's, that answer writes off an
        -- invoice, which is still payable, and a deposit, which arrives
        -- for it afterwards, is credited to nobody. It is parked instead,
        -- as an unknown one is, and the expiry, which follows, is what
        -- writes it off once it can no longer be paid
        Incoming -> park key payment
        -- nothing left the wallet, so everything, which was taken for it,
        -- goes back
        _ -> finish key payment Failed Nothing
               (walletPaymentAmountSats payment + reserveOf payment)
               "a payment, which the wallet did not make" now

    -- a payment the wallet does not have at all
    forget
      :: (MonadIO m, MonadMonitor m)
      => Key WalletPayment
      -> WalletPayment
      -> AppT m ()
    forget key payment = case walletPaymentDirection payment of
      -- an invoice of another wallet -- every invoice of a backend used
      -- before this one reads this way. It is written off once it can no
      -- longer be paid, and until then it is asked about again, as a
      -- wallet, which answers this way about an invoice it does have,
      -- would otherwise have it abandoned
      Incoming -> park key payment
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
            recordFailure key payment
          NoteUnknown -> do
            runLogging $ $(logError)
              ( "reconcilePendingPayments: the wallet does not have the "
              <> "outgoing payment "
              <> unPaymentHash (walletPaymentPaymentHash payment)
              <> " yet, whose balance is taken, so the wait before it is "
              <> "given back starts now"
              )
            touchWith key payment
              [ WalletPaymentUnknownSince =. Just now ] now
          WaitLonger -> do
            runLogging $ $(logError)
              ( "reconcilePendingPayments: the wallet does not have the "
              <> "outgoing payment "
              <> unPaymentHash (walletPaymentPaymentHash payment)
              <> " yet, whose balance is taken"
              )
            park key payment
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
            park key payment

    -- a payment the wallet is not finished with
    park
      :: (MonadIO m, MonadMonitor m)
      => Key WalletPayment
      -> WalletPayment
      -> AppT m ()
    park key payment = parkWith key payment []

    -- 'park' for an answer, which did have the payment: an answer from
    -- before, which did not, is no longer the state the payment is in, so
    -- a later one starts its own wait rather than carrying that one on
    parkKnown
      :: (MonadIO m, MonadMonitor m)
      => Key WalletPayment
      -> WalletPayment
      -> AppT m ()
    parkKnown key payment =
      parkWith key payment [ WalletPaymentUnknownSince =. Nothing ]

    -- An invoice, whose expiry and the grace after it have passed, is
    -- written off here: the wallet goes on answering that nobody paid it,
    -- so nothing but the expiry ever ends the asking. Anything else is
    -- only noted as asked about, which sends it to the back of the queue
    -- the next tick reads
    parkWith
      :: (MonadIO m, MonadMonitor m)
      => Key WalletPayment
      -> WalletPayment
      -> [Update WalletPayment]
      -> AppT m ()
    parkWith key payment extra = do
      State{ config = config, wallet = walletV } <- ask
      now <- liftIO getPOSIXTime
      let grace =
            fromIntegral (configWalletUnknownPaymentGraceSecs config)
      -- an invoice is only written off where the wallet, which issued it,
      -- is the wallet being asked. Who paid it does not matter -- anybody
      -- may -- but which wallet would have the record of it does: an
      -- invoice of a wallet used before this one reads as unknown here
      -- however it was paid, and writing it off as never paid loses a
      -- deposit, which arrived. Such an invoice is left for a person
      if walletPaymentDirection payment == Incoming
         && answeredByOriginatingWallet payment walletV
         && invoiceIsWrittenOff grace now (walletPaymentExpiresAt payment)
        then do
          runLogging $ $(logInfo)
            ( "reconcilePendingPayments: the invoice "
            <> unPaymentHash (walletPaymentPaymentHash payment)
            <> " expired without being paid and has not been paid since, "
            <> "so it is recorded as one, which was never paid"
            )
          finish key payment Expired Nothing (Sats 0) "" now
        else touchWith key payment extra now

    -- 'touch' for a caller, which has not read the clock
    touchNow
      :: (MonadIO m, MonadMonitor m)
      => Key WalletPayment
      -> WalletPayment
      -> AppT m ()
    touchNow key payment =
      liftIO getPOSIXTime >>= touch key payment

    -- records that the payment was asked about, without changing what it
    -- is. Only the order the next tick reads the payments in depends on it
    touch
      :: (MonadIO m, MonadMonitor m)
      => Key WalletPayment
      -> WalletPayment
      -> POSIXTime
      -> AppT m ()
    touch key payment now = touchWith key payment [] now

    -- 'touch', which also records something about the payment in the same
    -- statement, so the note and the place in the queue move together
    touchWith
      :: (MonadIO m, MonadMonitor m)
      => Key WalletPayment
      -> WalletPayment
      -> [Update WalletPayment]
      -> POSIXTime
      -> AppT m ()
    touchWith key payment extra now = do
      mtouched <- withDBTransaction ""
        -- the row itself, not its hash: two accounts may each hold a
        -- payment of one hash, and this is the one, which was read.
        -- Matched on when it started as well, as a row, which is taken up
        -- again, is the same row holding a different attempt
        $ updateWhereCount
          [ WalletPaymentId ==. key
          , WalletPaymentCreatedAt ==. walletPaymentCreatedAt payment
          , WalletPaymentStatus ==. Pending
          -- and the moment this tick took the row. Where a call to the
          -- wallet outlasted the interval, another tick has taken the row
          -- up and moved this on, and this answer is about the attempt as
          -- it was before that: it sends the row to the back of a queue
          -- it has already left
          , WalletPaymentUpdatedAt ==. walletPaymentUpdatedAt payment
          ]
          ([ WalletPaymentUpdatedAt =. now ] ++ extra)
      case mtouched of
        Just (_ :: Int64) -> return ()
        -- withDBTransaction has logged what went wrong
        Nothing -> runLogging $ $(logError)
          ( "reconcilePendingPayments: could not record that the payment "
          <> unPaymentHash (walletPaymentPaymentHash payment)
          <> " was asked about"
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
      :: (MonadIO m, MonadMonitor m)
      => Key WalletPayment
      -> WalletPayment
      -> WalletPaymentStatus
      -> Maybe Sats
      -> Sats
      -> Text
      -> POSIXTime
      -> AppT m ()
    finish key payment status mfee back reason now = do
      let paymentHashV = walletPaymentPaymentHash payment
          givesBack = walletPaymentDirection payment == Outgoing
                   && back > Sats 0
      mdone <- withDBTransaction "" $ do
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
          -- and the unknown timer this decision was taken on. Two ticks
          -- can be asking about one payment at once: where the other saw
          -- it pending it cleared this, so a tick, which is still holding
          -- the expired timer it read before that, would give the sats of
          -- a payment back, which the wallet has since taken up again.
          -- Matching the value, which was read, is what makes the later
          -- answer win rather than whichever tick writes last.
          --
          -- The attempt's own name is matched for the same reason: a tick,
          -- which asked by hash, is holding an answer about whatever that
          -- hash names, and a name recorded since makes that answer stale
          , WalletPaymentUnknownSince ==. walletPaymentUnknownSince payment
          , WalletPaymentCheckingId ==. walletPaymentCheckingId payment
          -- and the moment this tick took the row, which is what makes
          -- the holder of a stale answer lose. A call to the wallet,
          -- which outlasted the interval, leaves another tick free to
          -- take the row up, see it pending and move this on; without
          -- matching it the older call would still finish the payment --
          -- settling it, or failing it and giving its sats back -- on an
          -- answer about the attempt as it stood before
          , WalletPaymentUpdatedAt ==. walletPaymentUpdatedAt payment
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
      case mdone of
        Just () -> return ()
        -- withDBTransaction has logged what went wrong
        Nothing -> runLogging $ $(logError)
          ( "reconcilePendingPayments: the payment "
          <> unPaymentHash paymentHashV <> " is "
          <> walletPaymentStatusToText status
          <> " at the wallet and could not be recorded as such, so it is "
          <> "left pending to be tried again"
          )
