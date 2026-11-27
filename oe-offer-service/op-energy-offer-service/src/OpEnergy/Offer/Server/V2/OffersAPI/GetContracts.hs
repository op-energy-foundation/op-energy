{-- | GET /api/v1/offer/contracts
 -}
{-# LANGUAGE TemplateHaskell            #-}
module OpEnergy.Offer.Server.V2.OffersAPI.GetContracts
  ( getContracts
  , getContractsHandler
  , pageOffset
  ) where

import           Control.Monad.Trans.Except(ExceptT(..))
import           Control.Monad.Trans.Reader(ask)
import           Control.Monad.Trans(lift)
import           Control.Monad.IO.Class(liftIO)
import           Control.Monad.Logger(logError)
import qualified Control.Concurrent.STM.TVar as TVar

import           Database.Persist.Postgresql

import           Data.OpEnergy.API.V1.Positive(Positive, fromPositive)
import           Data.OpEnergy.Offer.API.V1.ContractStatus(ContractStatus)
import           Data.OpEnergy.Offer.API.V1.ContractInfo(PaginatedContracts(..))

import           OpEnergy.Offer.Server.V1.Class
                   ( AppM, State(..), profile, runLogging, withDBTransaction
                   )
import           OpEnergy.Offer.Server.V1.Offer

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

-- | the contracts of every offer, newest match first, optionally of one
-- status only, a page at a time.
--
-- A page is 'defaultLimit' contracts and never more than 'maxLimit', so a
-- caller can not ask for the whole table in one answer.
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
      -- newest match first, which is the order a contract listing is read
      -- in. The id breaks the tie of two contracts with the same matchedAt,
      -- which two accepts of one offer can get: without it a row can be
      -- skipped or repeated between two pages of this listing
      contractRows <- case pageOffset page limit of
        -- a page, whose offset does not fit, is past every row there is
        Nothing -> return []
        Just offset -> selectList statusFilter
          [ Desc ContractMatchedAt
          , Desc ContractId
          , LimitTo limit
          , OffsetBy offset
          ]
      return (totalCountV, contractRows)
  return $! PaginatedContracts
    { contracts = map (contractInfoFromEntity Nothing mTip) contractRows
    , page = fromIntegral page
    , limit = fromIntegral limit
    , totalCount = fromIntegral totalCountV
    }

-- | the OFFSET the given page of the listing is read with, or 'Nothing' when
-- the page is so far in that its offset can not be represented.
--
-- The offset is the number of whole pages before this one times the size of a
-- page. 'page' is a 'Positive' 'Int', so it is only bounded by @maxBound@, and
-- worked out in 'Int' that product wraps: page 4611686018427387905 of 100
-- wraps to offset 0 and answers the first page's contracts as if they were
-- that page's, and the largest page of all wraps to a negative offset, which
-- the database rejects. Worked out as 'Integer' and checked afterwards, so the
-- product being checked is not the one, which would have wrapped.
--
-- Example:
--
-- > pageOffset 3 20 == Just 40
pageOffset :: Int -> Int -> Maybe Int
pageOffset page limit
  | offset <= toInteger (maxBound :: Int) = Just (fromInteger offset)
  | otherwise = Nothing
  where
    offset = toInteger (page - 1) * toInteger limit
