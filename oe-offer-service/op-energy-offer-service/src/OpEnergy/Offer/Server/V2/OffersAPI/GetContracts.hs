{-- | GET /api/v1/offer/contracts
 -}
{-# LANGUAGE TemplateHaskell            #-}
module OpEnergy.Offer.Server.V2.OffersAPI.GetContracts
  ( getContracts
  , getContractsHandler
  ) where

import           Control.Monad.Trans.Reader(ask)
import           Control.Monad.Trans(lift)
import           Control.Monad.IO.Class(liftIO)
import           Control.Monad.Logger(logError)
import qualified Control.Concurrent.STM.TVar as TVar

import           Database.Persist.Postgresql

import           Data.OpEnergy.API.V1.Positive(Positive, fromPositive)
import           Data.OpEnergy.Offer.API.V1.ContractStatus(ContractStatus)
import           Data.OpEnergy.Offer.API.V1.ContractInfo(PaginatedContracts(..))

import           OpEnergy.Offer.Server.V1.Class(AppM, State(..), profile, runLogging)
import           OpEnergy.Offer.Server.V1.Offer

import           OpEnergy.Error(eitherThrowJSON, runExceptPrefixT, CallstackError)

defaultLimit, maxLimit :: Int
defaultLimit = 20
maxLimit = 100

getContractsHandler
  :: Maybe ContractStatus
  -> Maybe (Positive Int)
  -> Maybe (Positive Int)
  -> AppM PaginatedContracts
getContractsHandler mStatus mPage mLimit =
  let name = "V2.OffersAPI.GetContracts.getContractsHandler"
  in profile name $ eitherThrowJSON (runLogging . $(logError)) $ getContracts mStatus mPage mLimit

getContracts
  :: Maybe ContractStatus
  -> Maybe (Positive Int)
  -> Maybe (Positive Int)
  -> AppM (Either CallstackError PaginatedContracts)
getContracts mStatus mPage mLimit =
  let name = "V2.OffersAPI.GetContracts.getContracts"
  in profile name $ runExceptPrefixT name $ do
  State{ offerDBPool = pool, currentTip = currentTipV } <- lift ask
  mTip <- liftIO $ TVar.readTVarIO currentTipV
  let page = maybe 1 fromPositive mPage
      limit = maybe defaultLimit (min maxLimit . fromPositive) mLimit
      statusFilter = maybe [] (\s -> [ ContractStatus ==. s ]) mStatus
  liftIO $ flip runSqlPersistMPool pool $ do
    totalCountV <- count statusFilter
    -- newest match first, which is the order a contract listing is read in
    contractRows <- selectList statusFilter
      [ Desc ContractMatchedAt, LimitTo limit, OffsetBy ((page - 1) * limit) ]
    return $! PaginatedContracts
      { contracts = map (contractInfoFromEntity Nothing mTip) contractRows
      , page = fromIntegral page
      , limit = fromIntegral limit
      , totalCount = fromIntegral totalCountV
      }
