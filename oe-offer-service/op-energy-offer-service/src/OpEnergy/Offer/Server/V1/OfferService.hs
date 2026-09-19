{-- | Shared logic for cancel and expiry -- the only two places an Offer's
 - stake is ever refunded -- and the rule, shared by expiry and accept, of
 - which offers can still be accepted.
 -}
{-# LANGUAGE TemplateHaskell            #-}
module OpEnergy.Offer.Server.V1.OfferService
  ( closeOfferIfOpenTx
  , refundAndCloseOffer
  , notAcceptableAtFilter
  , isOfferAcceptableAt
  ) where

import           Control.Monad(join)
import           Control.Monad.Trans.Reader(ReaderT)
import           Control.Monad.IO.Class(MonadIO)
import           Control.Monad.Logger(logError)
import           Data.Int(Int64)
import           Data.Time.Clock(UTCTime)

import           Database.Persist.Postgresql
import           Prometheus(MonadMonitor)

import           Data.OpEnergy.API.V1.Block(BlockHeight)
import           Data.OpEnergy.API.V1.Natural(fromNatural)
import           Data.OpEnergy.Offer.API.V1.OfferStatus(OfferStatus(..))
import           OpEnergy.Offer.Server.V1.Class(AppT, runLogging, withDBTransaction)
import           OpEnergy.Offer.Server.V1.Offer
import qualified OpEnergy.Offer.Server.V1.AccountClient as AccountClient
import           Data.OpEnergy.Account.API.V1.Sats(Sats(..))
import           OpEnergy.Error(CallstackError, describeError)
import           Data.Text.Show(tshow)

-- | DB filter for the offers, which can not be accepted anymore at the
-- given chain tip: an offer can be accepted up to and including its
-- validTillBlock, and never once the tip has reached its target block. The
-- second condition matters for an offer whose validTillBlock is not before
-- its target block. Selects exactly the offers for which
-- 'isOfferAcceptableAt' is False: keep both in sync.
notAcceptableAtFilter :: BlockHeight -> [Filter Offer]
notAcceptableAtFilter tip =
  [ OfferValidTillBlock <. tip ] ||. [ OfferTargetBlock <=. tip ]

-- | Whether the given offer can still be accepted at the given chain tip:
-- up to and including its validTillBlock, and before the tip reaches its
-- target block. The opposite of 'notAcceptableAtFilter'.
isOfferAcceptableAt :: BlockHeight -> Offer -> Bool
isOfferAcceptableAt tip offerVal =
  tip <= offerValidTillBlock offerVal && tip < offerTargetBlock offerVal

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
-- treated as "not closed", so the caller can retry later. Returns the closed
-- offer with the result of the refund: the creator's new balance, or the
-- (already logged) error.
refundAndCloseOffer
  :: (MonadIO m, MonadMonitor m)
  => OfferId
  -> OfferStatus
  -> UTCTime
  -> AppT m (Maybe (Offer, Either CallstackError Sats))
refundAndCloseOffer offerId newStatus now = do
  mClosed <- join <$> withDBTransaction "closeOfferIfOpenTx"
    (closeOfferIfOpenTx offerId newStatus now)
  case mClosed of
    Nothing -> return Nothing
    Just offerVal -> do
      let unfilled = fromNatural (offerTotalContracts offerVal - offerMatchedCount offerVal)
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
      return $! Just (offerVal, ecredited)
