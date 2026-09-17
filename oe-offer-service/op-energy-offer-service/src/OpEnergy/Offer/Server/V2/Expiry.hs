{-- | Offer expiry sweep, run per scheduler tick.
 -}
{-# LANGUAGE TemplateHaskell #-}
module OpEnergy.Offer.Server.V2.Expiry
  ( expireStaleOffers
  ) where

import           Control.Monad(forM)
import           Control.Monad.IO.Class(liftIO, MonadIO)
import           Data.Maybe(fromMaybe)
import           Data.Time.Clock(getCurrentTime)

import           Database.Persist.Postgresql
import           Prometheus(MonadMonitor)

import           Data.OpEnergy.API.V1.Block(BlockHeight)
import           Data.OpEnergy.Offer.API.V1.OfferStatus(OfferStatus(..))

import           OpEnergy.Offer.Server.V1.Class(AppT, profile, withDBTransaction)
import           OpEnergy.Offer.Server.V1.Offer
import           OpEnergy.Offer.Server.V1.OfferService(refundAndCloseOffer)

expireStaleOffers :: (MonadIO m, MonadMonitor m) => BlockHeight -> AppT m Int
expireStaleOffers tipHeight =
  let name = "V2.Expiry.expireStaleOffers"
  in profile name $ do
  now <- liftIO getCurrentTime
  -- a failed query is logged by withDBTransaction; the sweep retries next tick
  staleOfferIds <- fromMaybe [] <$> withDBTransaction "selectKeysList"
    ( selectKeysList
      [ OfferStatus ==. Open, OfferTargetBlock <=. tipHeight ]
      []
    )
  results <- forM staleOfferIds $ \offerId -> refundAndCloseOffer offerId Expired now
  return $! length [ () | Just _ <- results ]
