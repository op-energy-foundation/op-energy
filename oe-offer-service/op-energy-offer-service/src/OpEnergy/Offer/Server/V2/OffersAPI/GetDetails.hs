{-- | GET /api/v1/offer/:id/details
 -}
{-# LANGUAGE TemplateHaskell            #-}
module OpEnergy.Offer.Server.V2.OffersAPI.GetDetails
  ( getDetails
  , getDetailsHandler
  ) where

import           Control.Monad.Trans.Reader(ask)
import           Control.Monad.Trans(lift)
import           Control.Monad.Trans.Except(throwE)
import           Control.Monad.IO.Class(liftIO)
import           Control.Monad.Logger(logError)
import qualified Control.Concurrent.STM.TVar as TVar
import           Data.Text(Text)
import qualified Data.Text as T
import qualified Data.Text.Read as TR

import           Database.Persist.Postgresql

import           Data.OpEnergy.Offer.API.V1.OfferID(OfferID(..))
import           Data.OpEnergy.Offer.API.V1.OfferDetails(OfferDetails(..))

import           OpEnergy.Offer.Server.V1.Class
                   ( AppM, State(..), profile, runLogging, withDBTransaction
                   )
import           OpEnergy.Offer.Server.V1.Offer

import           OpEnergy.Error
                   ( eitherThrowJSON, runExceptPrefixT, exceptTMaybeT
                   , CallstackError, invalidRequest, offerNotFound
                   , dbQueryError
                   )

-- | Servant-facing handler
getDetailsHandler :: OfferID -> AppM OfferDetails
getDetailsHandler (OfferID idText) =
  let name = "V2.OffersAPI.GetDetails.getDetailsHandler"
  in profile name $ eitherThrowJSON (runLogging . $(logError))
    $ getDetails idText

-- | the given offer with all its contracts, whatever the offer's status, so
-- that a closed offer's page can show them. Fails with 'invalidRequest'
-- (400) for an invalid id, with 'offerNotFound' (404) for an unknown offer
-- and with 'dbQueryError' (500) if the DB query fails.
--
-- The offer and its contracts are read in one DB transaction. At Postgres'
-- default isolation level, a contract matched between the two reads can be
-- listed while the offer's matchedCount and status do not include it yet.
--
-- Contracts come oldest first, in the order they were matched. An offer has
-- at most 'Data.OpEnergy.Offer.API.V1.Constants.maxContracts' of them, so
-- they are not paginated. The endpoint is public, so the contracts'
-- 'yourRole' is 'Nothing', as in the offer list.
getDetails :: Text -> AppM (Either CallstackError OfferDetails)
getDetails idText =
  let name = "V2.OffersAPI.GetDetails.getDetails"
  in profile name $ runExceptPrefixT name $ do
  key <- case TR.decimal idText of
    Right (n, rest) | T.null rest -> return (toSqlKey n :: OfferId)
    _ -> throwE $ invalidRequest "invalid offer id"
  State{ currentTip = currentTipV } <- lift ask
  (mofferVal, contractRows) <- exceptTMaybeT dbQueryError
    $ withDBTransaction "selectOfferDetails" $ do
      mofferVal <- get key
      contractRows <- selectList
        [ ContractOfferId ==. key ]
        [ Asc ContractMatchedAt ]
      return (mofferVal, contractRows)
  offerVal <- exceptTMaybeT offerNotFound $ return mofferVal
  mTip <- liftIO $ TVar.readTVarIO currentTipV
  return $! OfferDetails
    (offerInfoFromEntity (Entity key offerVal))
    (map (contractInfoFromEntity Nothing mTip) contractRows)
