{-- | Runs once per scheduler tick (see OpEnergy.Offer.Server.schedulerMainLoop)
 - against whatever chain tip is currently known. Silently a no-op with an
 - unknown tip (see OpEnergy.Offer.Server.V1.Class's currentTip field
 - comment -- this port never wires a live tip source, a known, flagged
 - gap), same permissive spirit as Post.post's own future-block check.
 -
 - Refunds every currently-"open" offer whose targetBlock has been reached
 - with no taker -- the predicted event has now happened (or its window
 - passed) with nobody on the other side, so there's nothing left to
 - settle; refunding and marking "expired" is the only correct outcome.
 -}
{-# LANGUAGE TemplateHaskell #-}
module OpEnergy.Offer.Server.V2.Expiry
  ( expireStaleOffers
  ) where

import           Control.Monad(forM)
import           Control.Monad.Trans.Reader(ask)
import           Control.Monad.IO.Class(liftIO, MonadIO)
import           Data.Time.Clock(getCurrentTime)

import           Database.Persist.Postgresql
import           Prometheus(MonadMonitor)

import           Data.OpEnergy.API.V1.Block(BlockHeight)
import           Data.OpEnergy.Offer.API.V1.OfferStatus(offerStatusOpen, offerStatusExpired)

import           OpEnergy.Offer.Server.V1.Class(AppT, State(..), profile)
import           OpEnergy.Offer.Server.V1.Offer(OfferStatus, OfferTargetBlock)
import           OpEnergy.Offer.Server.V1.OfferService(refundAndCloseOffer)

-- | returns how many offers were expired, for the caller (the scheduler
-- loop) to log. Finds the stale offer ids in one local query, then closes
-- each via refundAndCloseOffer -- which, per its own module's split, does
-- the actual status flip locally/atomically but the balance credit as a
-- separate cross-service call, so this can no longer be a single
-- runSqlPersistMPool block the way it was when the balance lived here too.
expireStaleOffers :: (MonadIO m, MonadMonitor m) => BlockHeight -> AppT m Int
expireStaleOffers tipHeight =
  let name = "V2.Expiry.expireStaleOffers"
  in profile name $ do
  State{ offerDBPool = pool } <- ask
  now <- liftIO getCurrentTime
  staleOfferIds <- liftIO $ flip runSqlPersistMPool pool $ selectKeysList
    [ OfferStatus ==. offerStatusOpen, OfferTargetBlock <=. tipHeight ]
    []
  results <- forM staleOfferIds $ \offerId -> refundAndCloseOffer offerId offerStatusExpired now
  return $! length [ () | Just _ <- results ]
