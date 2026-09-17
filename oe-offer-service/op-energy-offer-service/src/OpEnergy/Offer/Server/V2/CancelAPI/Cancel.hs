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
import           Control.Monad.IO.Class(liftIO)
import           Control.Monad.Logger(logError)
import           Data.Text(Text)
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import           Data.Time.Clock(getCurrentTime)

import           Database.Persist.Postgresql

import qualified Data.OpEnergy.Account.API.V1.Account as AccountAPI
import qualified Data.OpEnergy.Account.API.V2.WhoAmIResult as AccountV2
import           Data.OpEnergy.API.V1.Natural(fromNatural, verifyNatural)
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

  State{ offerDBPool = pool } <- lift ask
  offerVal <- exceptTMaybeT offerNotFound
    $! liftIO $ flip runSqlPersistMPool pool $ get key
  when (offerPersonUUID offerVal /= personUUIDV) $ throwE notOfferOwner
  when (offerStatus offerVal /= Open) $ throwE offerNotOpen

  now <- liftIO getCurrentTime
  let matched = fromNatural (offerMatchedCount offerVal)
      total   = fromNatural (offerTotalContracts offerVal)
      unfilled = total - matched
      refundAmount = offerMakerStakeSats offerVal * fromIntegral unfilled

  let (updates, updatedVal) = if matched == 0
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
  -- conditional update — only succeeds if neither the expiry sweep nor an
  -- accept has changed the offer since we read it above, so the refund below
  -- is never paid twice and never covers a slot that has been matched
  updated <- liftIO $ flip runSqlPersistMPool pool $ updateWhereCount
    [ OfferId ==. key
    , OfferStatus ==. Open
    , OfferMatchedCount ==. offerMatchedCount offerVal
    ]
    updates
  when (updated /= 1) $ throwE offerNotOpen

  -- refund unfilled stake
  ecredited <- lift $ AccountClient.creditBalance personUUIDV (Sats refundAmount)
  case ecredited of
    Right _ -> return ()
    Left err -> lift $ runLogging $ $(logError)
      ( "cancel: offer " <> tshow (fromSqlKey key)
      <> " closed but stake refund of " <> tshow refundAmount
      <> " sats failed, needs manual reconciliation: " <> describeError err
      )

  lift $ publishLiveEvent $! LiveEvent
    (LiveMessageOfferChanged
      (OfferID idText)
      (offerStatus updatedVal)
      (fromIntegral matched)
    )
    [personUUIDV]
  return $! offerInfoFrom idText updatedVal
