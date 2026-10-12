{-- | Offer expiry sweep, run per scheduler tick.
 -}
{-# LANGUAGE TemplateHaskell #-}
module OpEnergy.Offer.Server.V2.Expiry
  ( expireStaleOffers
  ) where

import           Control.Monad(forM, forM_)
import           Control.Monad.IO.Class(liftIO, MonadIO)
import           Control.Monad.Trans(lift)
import           Control.Monad.Trans.Except(runExceptT, ExceptT(..))

import           Database.Persist.Postgresql
import           Prometheus(MonadMonitor)

import           Data.OpEnergy.API.V1.Block(BlockHeight)
import           Data.OpEnergy.Offer.API.V1.OfferStatus(OfferStatus(..))
import           Data.OpEnergy.Offer.API.V1.LiveMessage(LiveMessage(..))

import           OpEnergy.Offer.Server.V1.Time(getCurrentTimeDB)
import           OpEnergy.Offer.Server.V1.Class(AppT, profile, withDBTransaction)

import           OpEnergy.Error
                 ( CallstackError, runExceptPrefixT
                 )
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
                   , withLiveEventOrderE
                   )

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
  now <- liftIO getCurrentTimeDB
  -- the sweep has nothing to work from when this fails, so it stops here
  -- instead of reading a failed query as "no offer is stale"
  staleOfferIds <- ExceptT $ withDBTransaction "selectKeysList"
    ( selectKeysList
      ( ( OfferStatus ==. Open ) : notAcceptableAtFilter tipHeight )
      []
    )
  results <- forM staleOfferIds $ \offerId -> withLiveEventOrderE $ do
    mclosed <- ExceptT $ refundAndCloseOffer offerId Expired now
    forM_ mclosed $ \(offerVal, ecredited) -> lift $ publishLiveEvent $! LiveEvent
      (LiveMessageOfferChanged (offerInfoFromEntity (Entity offerId offerVal)))
      (changedBalance (offerPersonUUID offerVal) ecredited)
    return mclosed
  return $! length [ () | (Just _)<- results ]
