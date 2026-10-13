{-- | GET /api/v1/offer/:id/details
 -}
{-# LANGUAGE TemplateHaskell            #-}
module OpEnergy.Offer.Server.V2.OffersAPI.GetDetails
  ( getDetails
  , getDetailsHandler
  ) where

import           Control.Monad.Trans.Reader(ask)
import           Control.Monad.Trans(lift)
import           Control.Monad.Trans.Except(ExceptT(..))
import           Control.Monad.IO.Class(liftIO)
import           Control.Monad.Logger(logError)
import qualified Control.Concurrent.STM.TVar as TVar
import           Data.Text(Text)

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
-- The offer and its contracts are read in one REPEATABLE READ transaction,
-- so both reads see the same snapshot: a contract, which is matched between
-- them, can not be listed while the offer's matchedCount and status do not
-- include it yet.
--
-- Contracts come oldest first, in the order they were matched, by their id
-- where two of them share a matchedAt. An offer has
-- at most 'Data.OpEnergy.Offer.API.V1.Constants.maxContracts' of them, so
-- they are not paginated. The endpoint is public, so the contracts'
-- 'yourRole' is 'Nothing', as in the offer list.
getDetails :: Text -> AppM (Either CallstackError OfferDetails)
getDetails idText =
  let name = "V2.OffersAPI.GetDetails.getDetails"
  in profile name $ runExceptPrefixT name $ do
  key <- exceptTMaybeT (invalidRequest "invalid offer id")
    $ return (offerKeyFromIDText idText)
  State{ currentUnconfirmedTip = currentUnconfirmedTipV } <- lift ask
  (mofferVal, contractRows) <- ExceptT $ withDBTransaction "selectOfferDetails" $ do
      -- both reads of this read-only transaction see one snapshot, so the
      -- contracts are the ones the offer's matchedCount counts
      rawExecute "SET TRANSACTION ISOLATION LEVEL REPEATABLE READ" []
      mofferVal <- get key
      contractRows <- selectList
        [ ContractOfferId ==. key ]
        -- the id breaks the tie of two contracts with the same matchedAt,
        -- which two accepts of one offer can get
        [ Asc ContractMatchedAt, Asc ContractId ]
      return (mofferVal, contractRows)
  offerVal <- exceptTMaybeT offerNotFound $ return mofferVal
  mTip <- liftIO $ TVar.readTVarIO currentUnconfirmedTipV
  return $! OfferDetails
    (offerInfoFromEntity (Entity key offerVal))
    (map (contractInfoFromEntity Nothing mTip) contractRows)
