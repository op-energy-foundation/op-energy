{-- | GET /api/v2/offer/list
 -}
{-# LANGUAGE TemplateHaskell            #-}
module OpEnergy.Offer.Server.V2.OffersAPI.GetList
  ( getList
  , getListHandler
  ) where

import           Control.Monad.Trans.Reader(ask)
import           Control.Monad.Trans(lift)
import           Control.Monad.IO.Class(liftIO)
import           Control.Monad.Logger(logError)
import           Database.Persist.Postgresql

import           Data.OpEnergy.API.V1.Positive(Positive, fromPositive)
import           Data.OpEnergy.Account.API.V1.Account(DisplayName)
import           Data.OpEnergy.Offer.API.V1.OfferStatus(OfferStatus)
import           Data.OpEnergy.Offer.API.V1.OfferInfo(PaginatedOffers(..))

import           OpEnergy.Offer.Server.V1.Class(AppM, State(..), profile, runLogging)
import           OpEnergy.Offer.Server.V1.Offer

import           OpEnergy.Error(eitherThrowJSON, runExceptPrefixT, CallstackError)

defaultLimit, maxLimit :: Int
defaultLimit = 20
maxLimit = 100

getListHandler
  :: Maybe OfferStatus
  -> Maybe DisplayName
  -> Maybe (Positive Int)
  -> Maybe (Positive Int)
  -> AppM PaginatedOffers
getListHandler mStatus mCreator mPage mLimit =
  let name = "V2.OffersAPI.GetList.getListHandler"
  in profile name $ eitherThrowJSON (runLogging . $(logError)) $ getList mStatus mCreator mPage mLimit

getList
  :: Maybe OfferStatus
  -> Maybe DisplayName
  -> Maybe (Positive Int)
  -> Maybe (Positive Int)
  -> AppM (Either CallstackError PaginatedOffers)
getList mStatus mCreator mPage mLimit =
  let name = "getList"
  in profile name $ runExceptPrefixT name $ do
  State{ offerDBPool = pool } <- lift ask
  let page = maybe 1 fromPositive mPage
      limit = maybe defaultLimit (min maxLimit . fromPositive) mLimit
      statusFilter = maybe [] (\s -> [ OfferStatus ==. s ]) mStatus
      creatorFilter = maybe [] (\d -> [ OfferCreatorDisplayName ==. d ]) mCreator
      filters = statusFilter ++ creatorFilter
  liftIO $ flip runSqlPersistMPool pool $ do
    totalCountV <- count filters
    rows <- selectList filters [ Desc OfferCreated, LimitTo limit, OffsetBy ((page - 1) * limit) ]
    return $! PaginatedOffers
      { items = map offerInfoFromEntity rows
      , page = fromIntegral page
      , limit = fromIntegral limit
      , totalCount = fromIntegral totalCountV
      }
