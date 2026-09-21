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
import           Control.Monad.Trans.Except(ExceptT(..), throwE)
import           Control.Monad.IO.Class(liftIO)
import           Control.Monad.Logger(logError)
import           Data.Text(Text)
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import           Data.Time.Clock(getCurrentTime)
import           Data.Int(Int64)

import           Database.Persist.Postgresql

import qualified Data.OpEnergy.Account.API.V1.Account as AccountAPI
import qualified Data.OpEnergy.Account.API.V2.WhoAmIResult as AccountV2
import           Data.OpEnergy.API.V1.Natural(fromNatural)
import           Data.OpEnergy.Offer.API.V1.OfferID(OfferID(..))
import           Data.OpEnergy.Offer.API.V1.OfferInfo(OfferInfo)
import           Data.OpEnergy.Offer.API.V1.OfferStatus(OfferStatus(..))
import           Data.Text.Show(tshow)

import           OpEnergy.Offer.Server.V1.Class(AppM, State(..), profile, runLogging)
import qualified OpEnergy.Offer.Server.V1.AccountClient as AccountClient
import           Data.OpEnergy.Account.API.V1.Sats(Sats(..))
import           OpEnergy.Offer.Server.V1.Offer
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
  let matched = offerMatchedCount offerVal
      total   = offerTotalContracts offerVal
      !unfilled = fromNatural (total - matched)
      refundAmount = offerMakerStakeSats offerVal * fromIntegral unfilled

  -- Atomic CAS: only update if status is still Open
  (updatedVal, changed) <- if fromNatural matched == (0 :: Int)
    then do
      bumped <- liftIO $ flip runSqlPersistMPool pool $
        updateWhereCount
          [ OfferId ==. key, OfferStatus ==. Open ]
          [ OfferStatus =. Cancelled
          , OfferRefundedAt =. Just now
          ]
      return (offerVal { offerStatus = Cancelled, offerRefundedAt = Just now }, bumped)
    else do
      bumped <- liftIO $ flip runSqlPersistMPool pool $
        updateWhereCount
          [ OfferId ==. key, OfferStatus ==. Open ]
          [ OfferTotalContracts =. matched
          , OfferStatus =. Filled
          , OfferRefundedAt =. Just now
          ]
      return ( offerVal
               { offerTotalContracts = matched
               , offerStatus = Filled
               , offerRefundedAt = Just now
               }
             , bumped
             )

  when (changed /= (1 :: Int64)) $ throwE offerNotOpen

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

  return $! offerInfoFrom idText updatedVal
