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
module OpEnergy.Offer.Server.V2.CancelAPI.Cancel
  ( cancel
  , cancelHandler
  ) where

import           Control.Monad.Trans.Reader(ask)
import           Control.Monad.Trans(lift)
import           Control.Monad.Trans.Except(ExceptT(..), throwE)
import           Control.Monad.IO.Class(MonadIO, liftIO)
import           Prometheus(MonadMonitor)
import           Control.Monad.Logger(logError)
import           Data.Text(Text)
import           Data.Time.Clock(UTCTime, getCurrentTime)
import           Data.Word(Word64)

import           Database.Persist.Postgresql

import qualified Data.OpEnergy.Account.API.V1.Account as AccountAPI
import qualified Data.OpEnergy.Account.API.V1.UUID as AccountAPI
import qualified Data.OpEnergy.Account.API.V2.WhoAmIResult as AccountV2
import           Data.OpEnergy.API.V1.Natural(fromNatural, verifyNatural)
import           Data.OpEnergy.Offer.API.V1.OfferID(OfferID(..))
import           Data.OpEnergy.Offer.API.V1.OfferInfo(OfferInfo)
import           Data.OpEnergy.Offer.API.V1.OfferStatus(OfferStatus(..))
import           Data.OpEnergy.Offer.API.V1.LiveMessage(LiveMessage(..))
import           Data.Text.Show(tshow)

import           OpEnergy.Offer.Server.V1.Class
                   ( AppM, AppT, State(..), profile, runLogging
                   )
import qualified OpEnergy.Offer.Server.V1.AccountClient as AccountClient
import           Data.OpEnergy.Account.API.V1.Sats(Sats(..))
import           OpEnergy.Offer.Server.V1.Offer
import           OpEnergy.Offer.Server.V1.LiveEvent
                   ( LiveEvent(..)
                   , changedBalance
                   )
import           OpEnergy.Offer.Server.V1.WebSocketService
                   ( publishLiveEvent
                   , withLiveEventOrderE
                   )
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
  key <- exceptTMaybeT (invalidRequest "invalid offer id")
    $ return (offerKeyFromIDText idText)

  (AccountV2.WhoAmIResult personUUIDV _displayName _balance) <-
    ExceptT $ AccountClient.verifyAccountToken token

  -- balances and offers change from here on: in withLiveEventOrder, so the
  -- change's events are published in the order of the changes
  withLiveEventOrderE $ do
    now <- liftIO getCurrentTime
    (updatedVal, refundAmount) <-
      closeForCancel key personUUIDV now cancelAttempts

    -- refund unfilled stake
    ecredited <- lift
      $ AccountClient.creditBalance personUUIDV (Sats refundAmount)
    case ecredited of
      Right _ -> return ()
      Left err -> lift $ runLogging $ $(logError)
        ( "cancel: offer " <> tshow (fromSqlKey key)
        <> " closed but stake refund of " <> tshow refundAmount
        <> " sats failed, needs manual reconciliation: " <> describeError err
        )

    let offerInfo = offerInfoFromEntity (Entity key updatedVal)
    lift $ publishLiveEvent $! LiveEvent
      (LiveMessageOfferChanged offerInfo)
      (changedBalance personUUIDV ecredited)
    return $! offerInfo

-- | how many times cancel reads the offer again, when a concurrent accept has
-- changed its matchedCount between the read and the update
cancelAttempts :: Int
cancelAttempts = 3

-- | closes the given account's open offer: fully when nothing is matched
-- yet, otherwise down to its matched contracts. The update only succeeds if
-- neither the expiry sweep nor an accept has changed the offer since it was
-- read, so the refund is never paid twice and never covers a matched slot.
-- If the offer is still open, it is read again, up to the given amount of
-- attempts. Returns the closed offer and the refund due to its creator.
closeForCancel
  :: (MonadIO m, MonadMonitor m)
  => OfferId
  -> AccountAPI.UUID AccountAPI.Person
  -> UTCTime
  -> Int
  -> ExceptT CallstackError (AppT m) (Offer, Word64)
closeForCancel key personUUIDV now attemptsLeft = do
  State{ offerDBPool = pool } <- lift ask
  offerVal <- exceptTMaybeT offerNotFound
    $! liftIO $ flip runSqlPersistMPool pool $ get key
  when (offerPersonUUID offerVal /= personUUIDV) $ throwE notOfferOwner
  when (offerStatus offerVal /= Open) $ throwE offerNotOpen
  let matched = fromNatural (offerMatchedCount offerVal)
      unfilled = fromNatural (offerTotalContracts offerVal) - matched
      refundAmount = offerMakerStakeSats offerVal * fromIntegral unfilled
      (updates, updatedVal) = if matched == 0
        then -- full cancel
          ( [ OfferStatus =. Cancelled
            , OfferRefundedAt =. Just now
            ]
          , offerVal { offerStatus = Cancelled, offerRefundedAt = Just now }
          )
        else -- partial cancel: reduce totalContracts to matchedCount, mark Filled
          ( [ OfferTotalContracts =. verifyNatural matched
            , OfferStatus =. Filled
            , OfferRefundedAt =. Just now
            ]
          , offerVal
            { offerTotalContracts = verifyNatural matched
            , offerStatus = Filled
            , offerRefundedAt = Just now
            }
          )
  updated <- liftIO $ flip runSqlPersistMPool pool $ updateWhereCount
    [ OfferId ==. key
    , OfferStatus ==. Open
    , OfferMatchedCount ==. offerMatchedCount offerVal
    ]
    updates
  if updated == 1
    then return (updatedVal, refundAmount)
    else do
      when (attemptsLeft <= 1) $ throwE offerNotOpen
      closeForCancel key personUUIDV now (attemptsLeft - 1)
