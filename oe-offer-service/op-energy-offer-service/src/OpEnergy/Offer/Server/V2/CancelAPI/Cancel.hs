{-- | POST /api/v1/offer/:id/cancel
 -
 - Cancellation behaviour depends on matched state:
 -
 - * @matchedCount == 0@: full cancel — refund @makerStakeSats * totalContracts@,
 -   set status to @Cancelled@.
 - * @matchedCount > 0@: partial cancel — reduce @totalContracts@ to
 -   @matchedCount@, refund the unfilled portion, set status to @Filled@.
 -   Existing contracts remain @Live@.
 -}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE BangPatterns               #-}
module OpEnergy.Offer.Server.V2.CancelAPI.Cancel
  ( cancel
  , cancelHandler
  ) where

import           Control.Monad.Trans.Reader(ask)
import           Control.Monad.Trans(lift)
import           Control.Monad.Trans.Except(ExceptT(..), runExceptT, throwE)
import           Control.Monad.IO.Class(MonadIO, liftIO)
import           Control.Monad.Logger(logError)
import           Data.Text(Text)
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import           Data.Time.Clock(UTCTime, getCurrentTime)
import           Data.Word(Word64)

import           Database.Persist.Postgresql

import qualified Data.OpEnergy.Account.API.V1.Account as AccountAPI
import qualified Data.OpEnergy.Account.API.V1.UUID as AccountAPI
import qualified Data.OpEnergy.Account.API.V2.WhoAmIResult as AccountV2
import           Data.OpEnergy.API.V1.Natural(fromNatural)
import           Data.OpEnergy.Offer.API.V1.OfferID(OfferID(..))
import           Data.OpEnergy.Offer.API.V1.OfferInfo(OfferInfo)
import           Data.OpEnergy.Offer.API.V1.OfferStatus(OfferStatus(..))
import           Data.OpEnergy.Offer.API.V1.LiveMessage(LiveMessage(..))
import           Data.Text.Show(tshow)

import           OpEnergy.Offer.Server.V1.Class(AppM, State(..), profile, runLogging)
import qualified OpEnergy.Offer.Server.V1.AccountClient as AccountClient
import           Data.OpEnergy.Account.API.V1.Sats(Sats(..))
import           OpEnergy.Offer.Server.V1.Offer
import           OpEnergy.Offer.Server.V1.LiveEvent(LiveEvent(..))
import           OpEnergy.Offer.Server.V1.WebSocketService(publishLiveEvent)
import           Control.Monad(when)

import           OpEnergy.Error
                   ( eitherThrowJSON, runExceptPrefixT
                   , exceptTMaybeT, describeError
                   , CallstackError, invalidRequest, offerNotFound
                   , notOfferOwner, offerNotOpen
                   )

cancelHandler :: OfferID -> AccountAPI.AccountToken -> AppM OfferInfo
cancelHandler (OfferID idText) token =
  let name = "V2.CancelAPI.Cancel.cancelHandler"
  in profile name $ eitherThrowJSON (runLogging . $(logError)) $ cancel idText token

cancel :: Text -> AccountAPI.AccountToken -> AppM (Either CallstackError OfferInfo)
cancel idText token =
  let name = "V2.CancelAPI.Cancel.cancel"
  in profile name $ runExceptPrefixT name $ do
  key <- case TR.decimal idText of
    Right (n, rest) | T.null rest -> return (toSqlKey n :: OfferId)
    _ -> throwE $ invalidRequest "invalid offer id"

  (AccountV2.WhoAmIResult personUUIDV _displayName _balance) <-
    ExceptT $ AccountClient.verifyAccountToken token

  now <- liftIO getCurrentTime
  (updatedVal, refundAmount) <- closeForCancel key personUUIDV now

  -- refund unfilled stake
  ecredited <- lift $ AccountClient.creditBalance personUUIDV (Sats refundAmount)
  case ecredited of
    Right _ -> return ()
    Left err -> do
      lift $ runLogging $ $(logError)
        ( "cancel: offer " <> tshow (fromSqlKey key)
        <> " closed but stake refund of " <> tshow refundAmount
        <> " sats failed, needs manual reconciliation: " <> describeError err
        )
      throwE $ invalidRequest
        ( "offer cancelled but refund of " <> tshow refundAmount
        <> " sats failed — contact support"
        )

  lift $ publishLiveEvent $! LiveEvent
    (LiveMessageOfferChanged
      (OfferID idText)
      (offerStatus updatedVal)
      (fromIntegral (fromNatural (offerMatchedCount updatedVal)))
    )
    [personUUIDV]
  return $! offerInfoFrom idText updatedVal

-- | closes the given account's open offer: fully when nothing is matched
-- yet, otherwise down to its matched contracts. Returns the closed offer and
-- the refund due to its creator.
closeForCancel
  :: OfferId
  -> AccountAPI.UUID AccountAPI.Person
  -> UTCTime
  -> ExceptT CallstackError AppM (Offer, Word64)
closeForCancel key personUUIDV now = do
  State{ offerDBPool = pool } <- lift ask
  ExceptT $ liftIO $ flip runSqlPersistMPool pool
    $ closeForCancelTx key personUUIDV now

-- | the read, the checks and the update of a cancel, in one transaction.
-- The offer is read FOR UPDATE, so an accept or the expiry sweep that wants
-- the same row waits for this transaction to commit instead of changing the
-- row in between. The checks therefore still hold when the update runs, which
-- lets the update address the offer by its primary key alone.
--
-- The refund itself is a call into the account service, so it stays outside
-- this transaction, in 'cancel'.
closeForCancelTx
  :: (MonadIO m)
  => OfferId
  -> AccountAPI.UUID AccountAPI.Person
  -> UTCTime
  -> SqlPersistT m (Either CallstackError (Offer, Word64))
closeForCancelTx key personUUIDV now = runExceptT $ do
  offerVal <- exceptTMaybeT offerNotFound $! getOfferForUpdate key
  when (offerPersonUUID offerVal /= personUUIDV) $ throwE notOfferOwner
  when (offerStatus offerVal /= Open) $ throwE offerNotOpen
  let matched = offerMatchedCount offerVal
      total   = offerTotalContracts offerVal
      !unfilled = fromNatural (total - matched)
      refundAmount = offerMakerStakeSats offerVal * fromIntegral unfilled
      (updates, updatedVal) = if fromNatural matched == (0 :: Int)
        then -- full cancel
          ( [ OfferStatus =. Cancelled
            , OfferRefundedAt =. Just now
            ]
          , offerVal { offerStatus = Cancelled, offerRefundedAt = Just now }
          )
        else -- partial cancel: reduce totalContracts to matchedCount, mark Filled
          ( [ OfferTotalContracts =. matched
            , OfferStatus =. Filled
            , OfferRefundedAt =. Just now
            ]
          , offerVal
            { offerTotalContracts = matched
            , offerStatus = Filled
            , offerRefundedAt = Just now
            }
          )
  lift $ update key updates
  return $! (updatedVal, refundAmount)

-- | reads an offer and keeps its row locked until the current transaction
-- ends. 'get' takes no lock, so a concurrent accept could raise matchedCount
-- between the read and the update and the refund would then cover a slot that
-- is already matched.
getOfferForUpdate
  :: (MonadIO m)
  => OfferId
  -> SqlPersistT m (Maybe Offer)
getOfferForUpdate key = do
  offers <- rawSql "SELECT ?? FROM offer WHERE id = ? FOR UPDATE"
    [ toPersistValue key ]
  return $! case offers of
    (Entity _ offerVal : _) -> Just offerVal
    _ -> Nothing
