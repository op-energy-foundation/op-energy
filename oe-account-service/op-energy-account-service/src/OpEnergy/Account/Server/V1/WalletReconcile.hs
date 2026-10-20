{-- | Asking the wallet what became of the payments, which are still
 - waiting.
 -
 - A wallet with a node of its own is not able to tell this service that an
 - invoice was paid: the sats arrive at the node, and nothing reaches this
 - process. So the scheduler asks, payment by payment, and records what it
 - is told. Crediting the balance is not done here: marking a payment
 - settled is all this does, and
 - 'OpEnergy.Account.Server.V1.WalletDeposits.creditWaitingDeposits'
 - credits exactly once from that, whichever of them arrives at the payment
 - first.
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

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Logger (logError, logInfo)
import           Control.Monad.Trans.Reader (ask)
import           Data.Int (Int64)
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           Database.Persist
import           Database.Persist.Postgresql (runSqlPersistMPool)
import           Database.Persist.Sql (updateWhereCount)
import           Prometheus (MonadMonitor)

import           Data.OpEnergy.Account.API.V2.PaymentHash (unPaymentHash)

import           OpEnergy.Account.Server.V1.Class
                 ( AppT, State(..), profile, runLogging
                 )
import           OpEnergy.Account.Server.V1.Wallet.Class
                 ( ObservedPayment(..), WalletBackend(..)
                 )
import           OpEnergy.Account.Server.V1.WalletPayment
import           OpEnergy.Error (eitherException)

-- | how many waiting payments one tick asks about. Each one is a call to
-- the wallet, so this bounds both the time a tick takes and the requests
-- the wallet is sent
maxPaymentsPerTick :: Int
maxPaymentsPerTick = 50

-- | asks the wallet about the incoming payments, which are still pending,
-- and records the answers. Run from the scheduler.
--
-- Only a payment, which is still pending, is changed, and only into the
-- state the wallet reports: an answer this service does not understand
-- leaves the payment alone, to be asked about again next tick, rather than
-- settling it on a guess
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
          [ WalletPaymentDirection ==. Incoming
          , WalletPaymentStatus ==. Pending
          ]
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
        Right (Left _) -> runLogging $ $(logError)
          ( "reconcilePendingPayments: the wallet could not answer about "
          <> "the payment " <> unPaymentHash paymentHashV
          )
        -- not paid yet, which is the usual answer: asked again next tick
        Right (Right ObservedPending) -> return ()
        Right (Right ObservedSettled) ->
          recordState pool paymentHashV Settled
        -- an invoice, which will not be paid, stops being asked about.
        -- Nothing credits a payment in this state, so recording it moves
        -- no balance
        Right (Right ObservedFailed) ->
          recordState pool paymentHashV Failed
        -- the wallet does not have this payment: it belongs to another
        -- wallet, which is what every payment of a backend, this service
        -- used before, looks like. It is written off once it can no
        -- longer be paid, and until then it is asked about again, as a
        -- wallet, which answers this way for a payment it does have,
        -- would otherwise have it abandoned
        Right (Right ObservedUnknown) -> do
          now <- liftIO getPOSIXTime
          if maybe False (< now) (walletPaymentExpiresAt payment)
            then do
              runLogging $ $(logInfo)
                ( "reconcilePendingPayments: the expired payment "
                <> unPaymentHash paymentHashV <> " is not the wallet's, so "
                <> "it is recorded as one, which was never paid"
                )
              recordState pool paymentHashV Expired
            else runLogging $ $(logInfo)
              ( "reconcilePendingPayments: the wallet does not have the "
              <> "payment " <> unPaymentHash paymentHashV
              <> ", which has not expired yet"
              )

    recordState pool paymentHashV status = do
      now <- liftIO getPOSIXTime
      erecorded <- liftIO $ eitherException $ flip runSqlPersistMPool pool
        -- conditional on the payment still being pending, so an answer,
        -- which arrives after the settle endpoint or another tick has
        -- already recorded one, changes nothing
        $ updateWhereCount
          [ WalletPaymentPaymentHash ==. paymentHashV
          , WalletPaymentStatus ==. Pending
          ]
          [ WalletPaymentStatus =. status
          , WalletPaymentUpdatedAt =. now
          ]
      case erecorded of
        Right (_ :: Int64) -> return ()
        Left err -> runLogging $ $(logError)
          ( "reconcilePendingPayments: the payment "
          <> unPaymentHash paymentHashV <> " is "
          <> walletPaymentStatusToText status
          <> " at the wallet and could not be recorded as such, so it "
          <> "needs manual reconciliation: " <> err
          )
