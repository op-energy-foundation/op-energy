{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
module OpEnergy.PagingResult
  ( pagingResult
  ) where

import qualified Data.List as List
import           Control.Monad.Trans.Reader (ask, ReaderT)

import qualified Data.Conduit as C
import           Data.Conduit ((.|), runConduit, ConduitT)
import qualified Data.Conduit.List as C
import           Control.Monad.Logger hiding (logDebug)
import           Control.Monad.Trans
import           Database.Persist.Pagination
import           Database.Persist.Postgresql
import           Control.Exception.Safe as E
import           Control.Monad.Trans.Resource

import           Data.OpEnergy.Account.API.V1.PagingResult
import           Data.OpEnergy.API.V1.Positive(fromPositive)
import           Data.OpEnergy.API.V1.Natural

import           OpEnergy.Account.Server.V1.Config (Config(..))
import           OpEnergy.Account.Server.V1.Class ( AppT, State(..))

pagingResult
  :: ( PersistEntity r
     , PersistField typ
     , PersistEntityBackend r ~ SqlBackend
     , Ord typ
     , MonadIO m
     , MonadCatch m
     )
  =>  Maybe (Natural Int)
  -> [Filter r]
  -> EntityField r typ
  -> ConduitT
    (Entity r)
    (r1)
    (ReaderT SqlBackend (NoLoggingT (ResourceT IO))) ()
  -> AppT m (Either SomeException (PagingResult r1) )
pagingResult mpage filter field next = E.handle (\(err::SomeException)-> return $ Left err) $ do
  State{ accountDBPool = pool
       , config = Config{ configRecordsPerReply = recordsPerReply}
       } <- ask
  eret <- liftIO $ flip runSqlPersistMPool pool $ do
    totalCount <- count (emptyList filter)
    if (fromNatural page) * (fromPositive recordsPerReply) >= totalCount
      then return (Right (totalCount, [])) -- page out of range
      else do
        pageResults <- runConduit
          $ streamEntities
            filter
            field
            (PageSize ((fromPositive recordsPerReply) + 1))
            Descend
            (Range Nothing Nothing)
          .| (C.drop (fromNatural page * fromPositive recordsPerReply) >> C.awaitForever C.yield) -- navigate to page
          .| next
          .| C.take (fromPositive recordsPerReply + 1) -- we take +1 to understand if there is a next page available
        return (Right (totalCount, pageResults))
  case eret of
    Left err -> return (Left err)
    Right (totalCount, resultsTail) -> do
      let newPage =
            if List.length resultsTail > fromPositive recordsPerReply
            then Just (fromIntegral (fromNatural page + 1))
            else Nothing
          results = List.take (fromPositive recordsPerReply) resultsTail
      return $ Right $ PagingResult
        { pagingResultNextPage = newPage
        , pagingResultCount = fromIntegral totalCount
        , pagingResultResults = results
        }
  where
    page = maybe 0 id mpage
    emptyList :: [Filter r] -> [Filter r]
    emptyList _ = []
