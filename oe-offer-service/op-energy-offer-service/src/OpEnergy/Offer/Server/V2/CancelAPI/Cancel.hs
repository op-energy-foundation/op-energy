{-- | POST /api/v2/offer/:id/cancel -- cancels the given offer. Only its
 - creator (via the AccountToken header) may, and only while it's still
 - "open" -- refunding its stake to the creator's balance (held by
 - oe-account-service) and returning the updated offer. See
 - OpEnergy.Offer.Server.V1.OfferService.refundAndCloseOffer for the
 - atomicity this actually gets (the status change is atomic and local; the
 - balance credit is a best-effort cross-service call after it).
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
import qualified Data.OpEnergy.Account.API.V2 as AccountV2
import           Data.OpEnergy.Offer.API.V2.OffersAPI(OfferInfo)
import           Data.OpEnergy.Offer.API.V1.OfferStatus(offerStatusCancelled)

import           OpEnergy.Offer.Server.V1.Class(AppM, State(..), profile, runLogging)
import qualified OpEnergy.Offer.Server.V1.AccountClient as AccountClient
import           OpEnergy.Offer.Server.V1.Offer(Offer(..), OfferId, offerInfoFrom)
import           OpEnergy.Offer.Server.V1.OfferService(refundAndCloseOffer)

import           OpEnergy.Error
                   ( eitherThrowJSON, runExceptPrefixT
                   , CallstackError, invalidRequest, offerNotFound, notOfferOwner, offerNotOpen
                   )

cancelHandler :: Text -> AccountAPI.AccountToken -> AppM OfferInfo
cancelHandler idText token =
  let name = "V2.CancelAPI.Cancel.cancelHandler"
  in profile name $ eitherThrowJSON (runLogging . $(logError)) $ cancel idText token

cancel :: Text -> AccountAPI.AccountToken -> AppM (Either CallstackError OfferInfo)
cancel idText token =
  let name = "cancel"
  in profile name $ runExceptPrefixT name $ do
  key <- case TR.decimal idText of
    Right (n, rest) | T.null rest -> return (toSqlKey n :: OfferId)
    _ -> throwE $ invalidRequest "invalid offer id"

  AccountV2.WhoAmIResult{ personUUID = personUUIDV } <- ExceptT $ AccountClient.verifyAccountToken token

  State{ offerDBPool = pool } <- lift ask
  mOffer <- liftIO $ flip runSqlPersistMPool pool $ get key
  case mOffer of
    Nothing -> throwE offerNotFound
    Just offerVal | offerPersonUUID offerVal /= personUUIDV -> throwE notOfferOwner
    _ -> return ()

  now <- liftIO getCurrentTime
  mUpdated <- lift $ refundAndCloseOffer key offerStatusCancelled now
  case mUpdated of
    Nothing -> throwE offerNotOpen
      -- ^ lost a race since the ownership check above -- someone else's
      -- concurrent cancel/expire got there first. Not "not found"/"not
      -- yours" (both already ruled out above), so 409 is still the right
      -- code -- see refundAndCloseOffer's own Nothing case.
    Just updatedVal -> return $! offerInfoFrom idText updatedVal
