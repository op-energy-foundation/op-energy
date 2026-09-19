{-- | Shared logic for cancel and expiry -- the only two places an Offer's
 - stake is ever refunded.
 -}
{-# LANGUAGE TemplateHaskell            #-}
module OpEnergy.Offer.Server.V1.OfferService
  ( closeOfferIfOpenTx
  , refundAndCloseOffer
  ) where

import           Control.Monad.Trans(lift)
import           Control.Monad.Trans.Except(ExceptT(..))
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
import           OpEnergy.Error
                 ( CallstackError, describeError, runExceptPrefixT
                 )
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
-- the unfilled portion of the offer.
--
-- A failed transaction is returned to the caller, which decides what a
-- failure means for it. @Right Nothing@ is the offer declining to close --
-- it is gone, already closed, or an accept has changed its matchedCount --
-- which is an answer rather than a failure.
refundAndCloseOffer
  :: (MonadIO m, MonadMonitor m)
  => OfferId
  -> OfferStatus
  -> UTCTime
  -> AppT m (Either CallstackError (Maybe Offer))
refundAndCloseOffer offerId newStatus now =
    let name = "V1.refundAndCloseOffer"
    in runExceptPrefixT name $ do
  mClosed <- ExceptT $ withDBTransaction "closeOfferIfOpenTx"
    (closeOfferIfOpenTx offerId newStatus now)
  case mClosed of
    Nothing -> return Nothing
    Just offerVal -> do
      let unfilled = fromNatural (offerTotalContracts offerVal - offerMatchedCount offerVal)
          refundAmount = offerMakerStakeSats offerVal * fromIntegral unfilled
      ecredited <- lift $ AccountClient.creditBalance (offerPersonUUID offerVal) (Sats refundAmount)
      case ecredited of
        Right _ -> return ()
        Left err -> lift $ runLogging $ $(logError)
          ( "refundAndCloseOffer: offer " <> tshow (fromSqlKey offerId)
          <> " closed (" <> tshow newStatus <> ") but its stake of " <> tshow refundAmount
          <> " sats was NOT refunded -- "
          <> "creditBalance failed, needs manual reconciliation: " <> describeError err
          )
      return $! Just (offerVal, ecredited)
