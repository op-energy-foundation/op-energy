{-- | Offer expiry sweep, run per scheduler tick.
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
import           OpEnergy.Offer.Server.V1.Offer
import           OpEnergy.Offer.Server.V1.OfferService(refundAndCloseOffer)

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
