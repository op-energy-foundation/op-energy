{-- | Offer expiry sweep, run per scheduler tick.
 -}
{-# LANGUAGE TemplateHaskell #-}
module OpEnergy.Offer.Server.V2.Expiry
  ( expireStaleOffers
  ) where

import           Control.Monad(forM, forM_)
import           Control.Monad.IO.Class(liftIO, MonadIO)
import           Control.Monad.Logger(logError)
import           Control.Monad.Trans(lift)
import           Control.Monad.Trans.Except(ExceptT(..))
import           Data.Time.Clock(getCurrentTime)

import           Database.Persist.Postgresql
import           Prometheus(MonadMonitor)

import           Data.OpEnergy.API.V1.Block(BlockHeight)
import           Data.OpEnergy.API.V1.Natural(fromNatural)
import           Data.OpEnergy.Offer.API.V1.OfferStatus(OfferStatus(..))
import           Data.Text.Show(tshow)
import           Data.OpEnergy.Offer.API.V1.LiveMessage(LiveMessage(..))

import           OpEnergy.Error
                 ( CallstackError, describeError, runExceptPrefixT
                 )
import           OpEnergy.Offer.Server.V1.Class
                 ( AppT, profile, runLogging, withDBTransaction
                 )
import           OpEnergy.Offer.Server.V1.Offer
import           OpEnergy.Offer.Server.V1.OfferService(refundAndCloseOffer)
import           OpEnergy.Offer.Server.V1.LiveEvent(LiveEvent(..))
import           OpEnergy.Offer.Server.V1.WebSocketService(publishLiveEvent)

-- | Closes every offer, whose target block the chain tip has passed, and
-- answers with the number closed.
--
-- A failure is returned rather than counted as nothing to do: the sweep
-- runs again on the next tick either way, but the scheduler is told the
-- difference between a tick, which found no stale offers, and a tick,
-- which could not look.
expireStaleOffers
  :: (MonadIO m, MonadMonitor m)
  => BlockHeight
  -> AppT m (Either CallstackError Int)
expireStaleOffers tipHeight =
    let name = "V2.Expiry.expireStaleOffers"
    in profile name $ runExceptPrefixT name $ do
  now <- liftIO getCurrentTime
  -- the sweep has nothing to work from when this fails, so it stops here
  -- instead of reading a failed query as "no offer is stale"
  staleOfferIds <- ExceptT $ withDBTransaction "selectKeysList"
    ( selectKeysList
      [ OfferStatus ==. Open, OfferTargetBlock <=. tipHeight ]
      []
    )
  results <- forM staleOfferIds $ \offerId -> do
    mclosed <- refundAndCloseOffer offerId Expired now
    forM_ mclosed $ \offerVal -> publishLiveEvent $! LiveEvent
      (LiveMessageOfferChanged
        (offerIDFromKey offerId)
        Expired
        (fromIntegral (fromNatural (offerMatchedCount offerVal)))
      )
      [offerPersonUUID offerVal]
    return mclosed
  return $! length [ () | Just _ <- results ]
