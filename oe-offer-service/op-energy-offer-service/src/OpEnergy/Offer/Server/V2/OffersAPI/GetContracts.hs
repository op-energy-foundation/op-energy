{-- | GET /api/v1/offer/contracts
 -}
{-# LANGUAGE TemplateHaskell            #-}
module OpEnergy.Offer.Server.V2.OffersAPI.GetContracts
  ( getContracts
  , getContractsHandler
  ) where

import           Control.Monad.Trans.Except(ExceptT(..))
import           Control.Monad.Trans.Reader(ask)
import           Control.Monad.Trans(lift)
import           Control.Monad.IO.Class(liftIO)
import           Control.Monad.Logger(logError)
import qualified Control.Concurrent.STM.TVar as TVar

import           Database.Persist.Postgresql

import           Data.OpEnergy.API.V1.Positive(Positive, fromPositive)
import           Data.OpEnergy.Offer.API.V1.ContractStatus(ContractStatus(..))
import           Data.OpEnergy.Offer.API.V1.ContractInfo(PaginatedContracts(..))

import           OpEnergy.Offer.Server.V1.Class
                   ( AppM, State(..), profile, runLogging, withDBTransaction
                   )
import           OpEnergy.Offer.Server.V1.Offer
import           OpEnergy.Offer.Server.V1.Paging(pageOffset)

import           OpEnergy.Error(eitherThrowJSON, runExceptPrefixT, CallstackError)

defaultLimit, maxLimit :: Int
defaultLimit = 20
maxLimit = 100

-- | servant handler of the contracts listing, which needs no account: a
-- contract is public, so the role of the caller is not part of the answer
getContractsHandler
  :: Maybe ContractStatus
  -> Maybe (Positive Int)
  -> Maybe (Positive Int)
  -> AppM PaginatedContracts
getContractsHandler mStatus mPage mLimit =
  let name = "V2.OffersAPI.GetContracts.getContractsHandler"
  in profile name $ eitherThrowJSON (runLogging . $(logError)) $ getContracts mStatus mPage mLimit

-- | the contracts of every offer, optionally of one status only, a page at
-- a time, in the order 'contractOrder' gives.
--
-- A page is 'defaultLimit' contracts and never more than 'maxLimit', so a
-- caller can not ask for the whole table in one answer.
-- | the order a page of contracts is read in, which is the order the
-- section showing it displays.
--
-- A contract, which has not settled yet, is read soonest first: what
-- settles next is what a reader of a live section is waiting on. One,
-- which has settled, is read most recently settled first, as a history is
-- read. A listing of no status in particular is read as a history.
--
-- The id breaks the tie of two contracts of the same target block, which
-- two accepts of one offer get: without it a row can be skipped or
-- repeated between two pages of this listing.
contractOrder :: Maybe ContractStatus -> [ SelectOpt Contract ]
contractOrder (Just Live) = [ Asc ContractTargetBlock, Asc ContractId ]
contractOrder _ = [ Desc ContractTargetBlock, Desc ContractId ]

getContracts
  :: Maybe ContractStatus
  -> Maybe (Positive Int)
  -> Maybe (Positive Int)
  -> AppM (Either CallstackError PaginatedContracts)
getContracts mStatus mPage mLimit =
  let name = "V2.OffersAPI.GetContracts.getContracts"
  in profile name $ runExceptPrefixT name $ do
  State{ currentUnconfirmedTip = currentUnconfirmedTipV } <- lift ask
  mTip <- liftIO $ TVar.readTVarIO currentUnconfirmedTipV
  let page = maybe 1 fromPositive mPage
      limit = maybe defaultLimit (min maxLimit . fromPositive) mLimit
      statusFilter = maybe [] (\s -> [ ContractStatus ==. s ]) mStatus
  (totalCountV, contractRows) <- ExceptT $ withDBTransaction "selectContracts" $ do
      -- both reads of this read-only transaction see one snapshot, so the
      -- totalCount is the count of the rows the page is taken from: at the
      -- default READ COMMITTED each statement takes its own snapshot, so an
      -- accept, which commits between them, makes the count describe a table
      -- the page was never read from
      rawExecute "SET TRANSACTION ISOLATION LEVEL REPEATABLE READ" []
      totalCountV <- count statusFilter
      contractRows <- case pageOffset page limit of
        -- a page, whose offset does not fit, is past every row there is
        Nothing -> return []
        Just offset -> selectList statusFilter
          ( contractOrder mStatus
         ++ [ LimitTo limit
            , OffsetBy offset
            ]
          )
      return (totalCountV, contractRows)
  return $! PaginatedContracts
    { contracts = map (contractInfoFromEntity Nothing mTip) contractRows
    , page = fromIntegral page
    , limit = fromIntegral limit
    , totalCount = fromIntegral totalCountV
    }
