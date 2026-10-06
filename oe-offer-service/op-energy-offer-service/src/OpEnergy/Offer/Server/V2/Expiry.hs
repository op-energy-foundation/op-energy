{-- | Offer expiry sweep, run per scheduler tick.
 -}
{-# LANGUAGE TemplateHaskell #-}
module OpEnergy.Offer.Server.V2.Expiry
  ( expireStaleOffers
  ) where

import           Control.Monad(forM, forM_)
import           Control.Monad.IO.Class(MonadIO)
import           Data.Maybe(fromMaybe)

import           Database.Persist.Postgresql
import           Prometheus(MonadMonitor)

import           Data.OpEnergy.API.V1.Block(BlockHeight)
import           Data.OpEnergy.Offer.API.V1.OfferStatus(OfferStatus(..))
import           Data.OpEnergy.Offer.API.V1.LiveMessage(LiveMessage(..))

import           OpEnergy.Offer.Server.V1.Time(getCurrentTimeDB)
import           OpEnergy.Offer.Server.V1.Class(AppT, profile, withDBTransaction)
import           OpEnergy.Offer.Server.V1.Offer
import           OpEnergy.Offer.Server.V1.OfferService
                   ( refundAndCloseOffer
                   , notAcceptableAtFilter
                   )
import           OpEnergy.Offer.Server.V1.LiveEvent
                   ( LiveEvent(..)
                   , changedBalance
                   )
import           OpEnergy.Offer.Server.V1.WebSocketService
                   ( publishLiveEvent
                   , withLiveEventOrder
                   )

-- | Expires the open offers, which can not be accepted anymore at the given
-- chain tip (see 'notAcceptableAtFilter'): once the tip has passed an
-- offer's validTillBlock or has reached its target block. Each is closed
-- with the matchedCount it has when it is closed, and its creator refunded
-- the stake of its unmatched contracts. An offer, whose matchedCount an
-- accept changes while it is being closed, is left to the next call; a
-- cancelled one is not expired; a failed refund is logged by
-- 'refundAndCloseOffer'. Returns the number of offers closed by this call.
expireStaleOffers :: (MonadIO m, MonadMonitor m) => BlockHeight -> AppT m Int
expireStaleOffers tipHeight =
  let name = "V2.Expiry.expireStaleOffers"
  in profile name $ do
  now <- getCurrentTimeDB
  -- a failed query is logged by withDBTransaction; the sweep retries next tick
  staleOfferIds <- fromMaybe [] <$> withDBTransaction "selectKeysList"
    ( selectKeysList
      ( [ OfferStatus ==. Open ] ++ notAcceptableAtFilter tipHeight )
      []
    )
  results <- forM staleOfferIds $ \offerId -> withLiveEventOrder $ do
    mclosed <- refundAndCloseOffer offerId Expired now
    forM_ mclosed $ \(offerVal, ecredited) -> publishLiveEvent $! LiveEvent
      (LiveMessageOfferChanged (offerInfoFromEntity (Entity offerId offerVal)))
      (changedBalance (offerPersonUUID offerVal) ecredited)
    return mclosed
  return $! length [ () | Just _ <- results ]
