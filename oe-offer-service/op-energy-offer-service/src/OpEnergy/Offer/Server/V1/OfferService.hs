{-- | Shared logic for cancel and expiry -- the only two places an Offer's
 - stake is ever refunded.
 -}
{-# LANGUAGE TemplateHaskell            #-}
module OpEnergy.Offer.Server.V1.OfferService
  ( closeOfferIfOpenTx
  , refundAndCloseOffer
  ) where

import           Control.Monad(join)
import           Control.Monad.Trans.Reader(ReaderT)
import           Control.Monad.IO.Class(MonadIO)
import           Control.Monad.Logger(logError)
import           Data.Int(Int64)
import           Data.Time.Clock(UTCTime)

import           Database.Persist.Postgresql
import           Prometheus(MonadMonitor)

import           Data.OpEnergy.API.V1.Natural(fromNatural)
import           Data.OpEnergy.Offer.API.V1.OfferStatus(OfferStatus(..))
import           OpEnergy.Offer.Server.V1.Class(AppT, runLogging, withDBTransaction)
import           OpEnergy.Offer.Server.V1.Offer
import qualified OpEnergy.Offer.Server.V1.AccountClient as AccountClient
import           Data.OpEnergy.Account.API.V1.Sats(Sats(..))
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
      | offerStatus offerVal /= Open -> return Nothing
      | otherwise -> do
          -- the refund is computed from this row's matchedCount, so the
          -- flip must fail if an accept has changed it in the meantime
          updated <- updateWhereCount
            [ OfferId ==. offerId
            , OfferStatus ==. Open
            , OfferMatchedCount ==. offerMatchedCount offerVal
            ]
            [ OfferStatus =. newStatus, OfferRefundedAt =. Just now ]
          if updated /= (1 :: Int64)
            then return Nothing
            else return $! Just offerVal { offerStatus = newStatus, offerRefundedAt = Just now }

-- | Full refund-and-close: local flip then cross-service credit.
-- Refunds @makerStakeSats * (totalContracts - matchedCount)@ — only
-- the unfilled portion of the offer. A failed DB transaction is logged and
-- treated as "not closed", so the caller can retry later.
refundAndCloseOffer
  :: (MonadIO m, MonadMonitor m)
  => OfferId
  -> OfferStatus
  -> UTCTime
  -> AppT m (Maybe Offer)
refundAndCloseOffer offerId newStatus now = do
  mClosed <- join <$> withDBTransaction "closeOfferIfOpenTx"
    (closeOfferIfOpenTx offerId newStatus now)
  case mClosed of
    Nothing -> return Nothing
    Just offerVal -> do
      let unfilled = fromNatural (offerTotalContracts offerVal) - fromNatural (offerMatchedCount offerVal)
          refundAmount = offerMakerStakeSats offerVal * fromIntegral unfilled
      ecredited <- AccountClient.creditBalance (offerPersonUUID offerVal) (Sats refundAmount)
      case ecredited of
        Right _ -> return ()
        Left err -> runLogging $ $(logError)
          ( "refundAndCloseOffer: offer " <> tshow (fromSqlKey offerId)
          <> " closed (" <> tshow newStatus <> ") but its stake of " <> tshow refundAmount
          <> " sats was NOT refunded -- "
          <> "creditBalance failed, needs manual reconciliation: " <> describeError err
          )
      return $! Just offerVal
