{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
module OpEnergy.PagingResult
  ( pagingResult
  ) where

import qualified Data.List as List
import           Control.Monad.Trans.Reader ( ReaderT)

import qualified Data.Conduit as C
import           Data.Conduit ((.|), runConduit, ConduitT)
import qualified Data.Conduit.List as C
import           Control.Monad.Logger hiding (logDebug)
import           Control.Monad.Trans
import           Database.Persist.Pagination
import           Database.Persist.Postgresql
import           Control.Exception.Safe as E
import           Control.Monad.Trans.Resource
import           Prometheus(MonadMonitor(..))

import           Data.OpEnergy.Account.API.V1.PagingResult
import           Data.OpEnergy.API.V1.Positive(Positive, fromPositive)
import           Data.OpEnergy.API.V1.Natural

import           OpEnergy.Account.Server.V1.Class ( AppT, profile, withDBTransaction)

pagingResult
  :: ( PersistEntity r
     , PersistField typ
     , PersistEntityBackend r ~ SqlBackend
     , Ord typ
     , MonadIO m
     , MonadCatch m
     , MonadMonitor m
     )
  =>  Maybe (Natural Int)
  -> Positive Int
  -> [Filter r]
  -> SortOrder
  -> EntityField r typ
  -> ConduitT
    (Entity r)
    (r1)
    (ReaderT SqlBackend (NoLoggingT (ResourceT IO))) ()
  -> AppT m (Maybe (PagingResult r1) )
pagingResult mpage recordsPerReply filter sortOrder field next = profile "pagingResult" $ do
  mret <- withDBTransaction "" $ do
    pageResults <- runConduit
      $ streamEntities
        filter
        field
        (PageSize ((fromPositive recordsPerReply) + 1))
        sortOrder
        (Range Nothing Nothing)
      .| next
      .| (C.drop (fromNatural page * fromPositive recordsPerReply) >> C.awaitForever C.yield) -- navigate to page
      .| C.take (fromPositive recordsPerReply + 1) -- we take +1 to understand if there is a next page available
    return (pageResults)
  case mret of
    Nothing -> return Nothing
    Just (resultsTail) -> do
      let newPage =
            if List.length resultsTail > fromPositive recordsPerReply
            then Just (fromIntegral (fromNatural page + 1))
            else Nothing
          results = List.take (fromPositive recordsPerReply) resultsTail
      return $ Just $ PagingResult
        { pagingResultNextPage = newPage
        , pagingResultResults = results
        }
  where
    page = maybe 0 id mpage

