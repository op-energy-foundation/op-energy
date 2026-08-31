{-- | Shared logic for cancel and expiry -- the only two places an Offer's
 - stake is ever refunded.
 -}
{-# LANGUAGE TemplateHaskell            #-}
module OpEnergy.Offer.Server.V1.OfferService
  ( closeOfferIfOpenTx
  , refundAndCloseOffer
  ) where

import           Control.Monad.Trans.Reader(ReaderT, ask)
import           Control.Monad.IO.Class(MonadIO, liftIO)
import           Control.Monad.Logger(logError)
import           Data.Int(Int64)
import           Data.Time.Clock(UTCTime)

import           Database.Persist.Postgresql

import           Data.OpEnergy.Offer.API.V1.OfferStatus(OfferStatus, offerStatusOpen)
import           OpEnergy.Offer.Server.V1.Class(AppT, State(..), runLogging)
import           OpEnergy.Offer.Server.V1.Offer
import qualified OpEnergy.Offer.Server.V1.AccountClient as AccountClient
import           OpEnergy.Error(describeError)
import           Data.Text.Show(tshow)

-- | Idempotent, atomic, local-only status flip
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

-- | Full refund-and-close: local flip then cross-service credit
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
