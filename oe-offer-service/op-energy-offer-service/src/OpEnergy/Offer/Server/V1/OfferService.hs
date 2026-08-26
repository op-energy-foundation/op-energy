{-- | Shared logic used by both OpEnergy.Offer.Server.V2.CancelAPI.Cancel
 - and OpEnergy.Offer.Server.V2.Expiry -- the only two places an Offer's
 - stake is ever refunded. Named/placed the same way
 - OpEnergy.Account.Server.V1.AccountService holds lookups
 - (mgetPersonByAccountToken etc.) shared across that service's V1 and V2
 - handlers: a V1-namespaced helper layer beneath the V2 per-endpoint
 - handlers, even though the offers it operates on only ever existed as a
 - V2 feature.
 -
 - Split into two pieces because the balance it refunds no longer lives in
 - this service's own DB (see OpEnergy.Offer.Server.V1.AccountClient's
 - module header): 'closeOfferIfOpenTx' is the local, atomic race guard
 - (purely a DB transaction); 'refundAndCloseOffer' runs that and then, only
 - if it won, makes the cross-service call to actually credit the balance.
 - That second step can no longer be part of the same atomic transaction as
 - the first -- see 'refundAndCloseOffer's own comment for the accepted
 - consequence.
 -}
{-# LANGUAGE TemplateHaskell            #-}
module OpEnergy.Offer.Server.V1.OfferService
  ( closeOfferIfOpenTx
  , refundAndCloseOffer
  ) where

import           Control.Monad.Trans.Reader(ask)
import           Control.Monad.IO.Class(MonadIO, liftIO)
import           Control.Monad.Logger(logError)
import           Data.Int(Int64)
import           Data.Time.Clock(UTCTime)

import           Database.Persist.Postgresql

import           Data.OpEnergy.Offer.API.V1.OfferStatus(OfferStatus, offerStatusOpen)
import           OpEnergy.Offer.Server.V1.Class(AppT, State(..), runLogging)
import           OpEnergy.Offer.Server.V1.Offer(Offer(..), OfferId)
import qualified OpEnergy.Offer.Server.V1.AccountClient as AccountClient
import           OpEnergy.Error(describeError)
import           Data.Text.Show(tshow)

-- | Idempotent, atomic, local-only: attempts to move offerId from "open" to
-- newStatus, stamping refundedAt, as a single conditional UPDATE
-- ("status == open") -- the actual race guard, same pattern as
-- OpEnergy.Offer.Server.V2.PostOfferAPI.Post's balance deduction used to be
-- before it moved cross-service. Run this inside the caller's own
-- runSqlPersistMPool block -- it does not open its own transaction.
-- Returns Nothing (a safe no-op, never double-refunding) if the offer
-- doesn't exist or isn't "open" right now -- already closed by a previous
-- call, or lost a race to a concurrent one.
closeOfferIfOpenTx
  :: (MonadIO m)
  => OfferId
  -> OfferStatus
  -> UTCTime
  -> ReaderT SqlBackend m (Maybe Offer)
closeOfferIfOpenTx offerId newStatus now = do
  mOffer <- get offerId
  case mOffer of
    Nothing -> return Nothing
    Just offerVal
      | offerStatus offerVal /= offerStatusOpen -> return Nothing
      | otherwise -> do
          updated <- updateWhereCount
            [ OfferId ==. offerId, OfferStatus ==. offerStatusOpen ]
            [ OfferStatus =. newStatus, OfferRefundedAt =. Just now ]
          if updated /= (1 :: Int64)
            then return Nothing
            else return $! Just offerVal { offerStatus = newStatus, offerRefundedAt = Just now }

-- | Full refund-and-close: 'closeOfferIfOpenTx' (its own transaction),
-- then -- only if that won the race -- a cross-service credit of the
-- offer's stake back to its creator's balance
-- (OpEnergy.Offer.Server.V1.AccountClient.creditBalance).
--
-- The credit is best-effort: if the local flip already succeeded and the
-- cross-service call then fails (network error, oe-account-service down),
-- this logs loudly and returns the closed offer anyway, rather than
-- leaving it "open" forever or trying to undo the local flip (undoing it
-- would race a concurrent expiry/cancel the same way the flip itself
-- guards against). The accepted risk is a closed-but-not-yet-refunded
-- offer, recoverable by a manual/future reconciliation pass -- not
-- attempted here. See docs/plans/post-offer-api.md's "known gaps".
refundAndCloseOffer
  :: (MonadIO m)
  => OfferId
  -> OfferStatus
  -> UTCTime
  -> AppT m (Maybe Offer)
refundAndCloseOffer offerId newStatus now = do
  State{ offerDBPool = pool } <- ask
  mClosed <- liftIO $ flip runSqlPersistMPool pool $ closeOfferIfOpenTx offerId newStatus now
  case mClosed of
    Nothing -> return Nothing
    Just offerVal -> do
      ecredited <- AccountClient.creditBalance (offerPersonUUID offerVal) (offerMakerStakeSats offerVal)
      case ecredited of
        Right _ -> return ()
        Left err -> runLogging $ $(logError)
          ( "refundAndCloseOffer: offer " <> tshow (fromSqlKey offerId)
          <> " closed (" <> tshow newStatus <> ") but its stake was NOT refunded -- "
          <> "creditBalance failed, needs manual reconciliation: " <> describeError err
          )
      return $! Just offerVal
