{-- | POST /api/v2/offer/:id/cancel
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
import           Data.OpEnergy.Offer.API.V1.OfferInfo(OfferID(..), OfferInfo)
import           Data.OpEnergy.Offer.API.V1.OfferStatus(OfferStatus(..))

import           OpEnergy.Offer.Server.V1.Class(AppM, State(..), profile, runLogging)
import qualified OpEnergy.Offer.Server.V1.AccountClient as AccountClient
import           OpEnergy.Offer.Server.V1.Offer(Offer(..), OfferId, offerInfoFrom)
import           OpEnergy.Offer.Server.V1.OfferService(refundAndCloseOffer)

import           OpEnergy.Error
                   ( eitherThrowJSON, runExceptPrefixT
                   , CallstackError, invalidRequest, offerNotFound, notOfferOwner, offerNotOpen
                   )

cancelHandler :: OfferID -> AccountAPI.AccountToken -> AppM OfferInfo
cancelHandler (OfferID idText) token =
  let name = "V2.CancelAPI.Cancel.cancelHandler"
  in profile name $ eitherThrowJSON (runLogging . $(logError)) $ cancel idText token

cancel :: Text -> AccountAPI.AccountToken -> AppM (Either CallstackError OfferInfo)
cancel idText token =
  let name = "cancel"
  in profile name $ runExceptPrefixT name $ do
  key <- case TR.decimal idText of
    Right (n, rest) | T.null rest -> return (toSqlKey n :: OfferId)
    _ -> throwE $ invalidRequest "invalid offer id"

  (AccountV2.WhoAmIResult personUUIDV _displayName _balance) <- ExceptT $ AccountClient.verifyAccountToken token

  State{ offerDBPool = pool } <- lift ask
  mOffer <- liftIO $ flip runSqlPersistMPool pool $ get key
  case mOffer of
    Nothing -> throwE offerNotFound
    Just offerVal | offerPersonUUID offerVal /= personUUIDV -> throwE notOfferOwner
    _ -> return ()

  now <- liftIO getCurrentTime
  mUpdated <- lift $ refundAndCloseOffer key Cancelled now
  case mUpdated of
    Nothing -> throwE offerNotOpen
    Just updatedVal -> return $! offerInfoFrom idText updatedVal
