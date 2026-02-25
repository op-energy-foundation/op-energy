{-- | This module implements BlockTime strike service.
 -}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE EmptyDataDecls          #-}
{-# LANGUAGE GADTs                     #-}
module OpEnergy.BlockTimeStrike.Server.V1.BlockTimeStrikeService.GetBlockTimeStrikesPage
  ( getBlockTimeStrikesPage
  , getBlockTimeStrikesPageHandler
  , maybeFetchObservedStrike
  , fetchStrikeByGuessesCount
  , maybeFetchGuessesCount
  ) where

import           Servant ( err500, ServerError)
import           Control.Monad.Trans.Reader ( asks, ReaderT)
import           Control.Monad.Logger( logError,  NoLoggingT)
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TVar as TVar
import           Data.Text (Text)
import           Data.Conduit( (.|) )
import qualified Data.Conduit.List as C
import           Control.Monad.Trans
import           Control.Monad.Trans.Except
                   ( runExceptT, ExceptT (..))
import           Control.Monad.Trans.Resource( ResourceT)
import           Data.Maybe( fromMaybe)

import           Database.Persist
import           Database.Persist.Sql
import           Database.Persist.Pagination
import           Prometheus(MonadMonitor)


import           Data.OpEnergy.API.V1.Natural
import qualified Data.OpEnergy.Account.API.V1.BlockTimeStrike            as API
import qualified Data.OpEnergy.Account.API.V1.PagingResult               as API
import qualified Data.OpEnergy.Account.API.V1.FilterRequest              as API
import qualified Data.OpEnergy.Account.API.V1.BlockTimeStrikeFilterClass as API

import           OpEnergy.ExceptMaybe(exceptTMaybeT)
import           OpEnergy.Error( eitherThrowJSON, runExceptPrefixT)
import           OpEnergy.PagingResult
import qualified OpEnergy.BlockTimeStrike.Server.V1.Class as BlockTime
import qualified OpEnergy.BlockTimeStrike.Server.V1.BlockTimeStrikeFilter as BlockTimeStrikeFilter
import           OpEnergy.BlockTimeStrike.Server.V1.BlockTimeStrike
import           OpEnergy.BlockTimeStrike.Server.V1.BlockTimeStrikeGuess
import qualified OpEnergy.BlockTimeStrike.Server.V1.SlowFast as SlowFast
import           OpEnergy.Account.Server.V1.Config
                 as Config(Config(..))
import           OpEnergy.Account.Server.V1.Class (AppT, AppM, State(..), runLogging, profile )


data IsSortByGuessesCountNeeded
  = SortByGuessesCountNotNeeded
  | SortByGuessesCountNeeded

-- | returns list of BlockTimeStrike records
getBlockTimeStrikesPageHandler
  :: Maybe (Natural Int)
  -> Maybe (API.FilterRequest API.BlockTimeStrike API.BlockTimeStrikeFilter)
  -> AppM (API.PagingResult API.BlockTimeStrikeWithGuessesCount)
getBlockTimeStrikesPageHandler mpage mfilterAPI =
    let name = "V1.getBlockTimeStrikesPageHandler"
    in profile name $ do
  eitherThrowJSON
    (\reason-> do
      callstack <- asks callStack
      let msg = callstack <> ": " <> reason
      runLogging $ $(logError) msg
      return msg
    )
    $ runExceptPrefixT name $ ExceptT $ getBlockTimeStrikesPage mpage mfilterAPI

getBlockTimeStrikesPage
  :: ( MonadIO m
     , MonadMonitor m
     )
  => Maybe (Natural Int)
  -> Maybe (API.FilterRequest API.BlockTimeStrike API.BlockTimeStrikeFilter)
  -> AppT m (Either (ServerError, Text) (API.PagingResult API.BlockTimeStrikeWithGuessesCount))
getBlockTimeStrikesPage mpage mfilterAPI =
    let name = "getBlockTimeStrikesPage"
    in profile name $ runExceptPrefixT name $ do
  latestUnconfirmedBlockHeightV <- lift $ asks (BlockTime.latestUnconfirmedBlockHeight . blockTimeState)
  configBlockTimeStrikeGuessMinimumBlockAheadCurrentTip <- lift $ asks (configBlockTimeStrikeGuessMinimumBlockAheadCurrentTip . config)
  latestConfirmedBlockV <- lift $ asks (BlockTime.latestConfirmedBlock . blockTimeState)
  (latestUnconfirmedBlockHeight, latestConfirmedBlock) <-
    ExceptT $ liftIO $ STM.atomically $ runExceptT $ (,)
      <$> (exceptTMaybeT ( err500, "latest unconfirmed block hasn't been received yet")
          $ TVar.readTVar latestUnconfirmedBlockHeightV
          )
      <*> (exceptTMaybeT (err500, "latest confirmed block hasn't been received yet")
          $ TVar.readTVar latestConfirmedBlockV
          )
  let
      staticPartFilter = maybe
                           []
                           ( API.buildFilter
                           . API.unFilterRequest
                           . API.mapFilter
                           )
                           mfilter
      strikeFilter =
        BlockTimeStrikeFilter.buildFilterByClass
          ( maybe
            Nothing
            ( API.blockTimeStrikeFilterClass
            . fst
            . API.unFilterRequest
            ) mfilter
          )
          latestUnconfirmedBlockHeight
          latestConfirmedBlock
          configBlockTimeStrikeGuessMinimumBlockAheadCurrentTip
        ++ staticPartFilter
  exceptTMaybeT (err500, "getBlockTimeStrikePast failed")
    $ getBlockTimeStrikePast strikeFilter
  where
    mfilter = fmap coerceFilterRequestBlockTimeStrike mfilterAPI
    sort = maybe
             Descend
             ( API.sortOrder
             . API.unFilterRequest
             )
             mfilter
    getBlockTimeStrikePast
      :: ( MonadIO m
         , MonadMonitor m
         )
      => [Filter BlockTimeStrike]
      -> ReaderT
         State
         m
         (Maybe (API.PagingResult API.BlockTimeStrikeWithGuessesCount))
    getBlockTimeStrikePast strikeFilter = do
      recordsPerReply <- asks (configRecordsPerReply . config)
      let
          linesPerPage = maybe
            recordsPerReply
            ( fromMaybe recordsPerReply
            . API.blockTimeStrikeFilterLinesPerPage
            . fst
            . API.unFilterRequest
            )
            mfilter
      let
          eGuessesCount = case maybe
                API.StrikeSortOrderDescend
                ( fromMaybe API.StrikeSortOrderDescend
                . API.blockTimeStrikeFilterSort
                . fst
                . API.unFilterRequest
                )
                mfilter
              of
            API.StrikeSortOrderAscend -> SortByGuessesCountNotNeeded
            API.StrikeSortOrderDescend -> SortByGuessesCountNotNeeded
            API.StrikeSortOrderAscendGuessesCount ->  SortByGuessesCountNeeded
            API.StrikeSortOrderDescendGuessesCount -> SortByGuessesCountNeeded
      case eGuessesCount of
        SortByGuessesCountNotNeeded -> pagingResult
          mpage
          linesPerPage
          strikeFilter
          sort
          BlockTimeStrikeId -- select strikes with given filter first
          $  C.concatMapM maybeFetchGuessesCount
          .| C.concatMapM (maybeFetchObservedStrike mfilter)
          .| C.map renderBlockTimeStrike
        SortByGuessesCountNeeded -> pagingResult
          mpage
          linesPerPage
          []
          sort
          CalculatedBlockTimeStrikeGuessesCountGuessesCount
          $  C.concatMapM (fetchStrikeByGuessesCount strikeFilter)
          .| C.concatMapM (maybeFetchObservedStrike mfilter)
          .| C.map renderBlockTimeStrike
maybeFetchGuessesCount
  :: Entity BlockTimeStrike
  -> (ReaderT SqlBackend (NoLoggingT (ResourceT IO)))
     [( Entity CalculatedBlockTimeStrikeGuessesCount
      , Entity BlockTimeStrike
      )
     ]
maybeFetchGuessesCount strikeE@(Entity strikeId _) = do
  mguessesCount <- selectFirst
    [ CalculatedBlockTimeStrikeGuessesCountStrike ==. strikeId]
    []
  case mguessesCount of
    Just guessesCount -> return [(guessesCount, strikeE )]
    Nothing -> do -- guesses count should exist. if it's not, then this is unexpected
      return []
fetchStrikeByGuessesCount
  :: [Filter BlockTimeStrike]
  -> Entity CalculatedBlockTimeStrikeGuessesCount
  -> (ReaderT SqlBackend (NoLoggingT (ResourceT IO)))
     [(Entity CalculatedBlockTimeStrikeGuessesCount, Entity BlockTimeStrike)]
fetchStrikeByGuessesCount strikeFilter guessE@(Entity _ guessesCount) = do
  mstrike <- selectFirst
    ((BlockTimeStrikeId ==. calculatedBlockTimeStrikeGuessesCountStrike guessesCount)
     :strikeFilter
    )
    []
  case mstrike of
    Just strikeE-> return [(guessE, strikeE)]
    Nothing -> return []
maybeFetchObservedStrike
  :: Maybe (API.FilterRequest BlockTimeStrike API.BlockTimeStrikeFilter)
  -> ( Entity CalculatedBlockTimeStrikeGuessesCount
     , Entity BlockTimeStrike
     )
  -> (ReaderT SqlBackend (NoLoggingT (ResourceT IO)))
     [( Entity BlockTimeStrike
      , Maybe (Entity BlockTimeStrikeObserved)
      , Entity CalculatedBlockTimeStrikeGuessesCount
      )
     ]
maybeFetchObservedStrike mfilter (guessesCount, strikeE@(Entity strikeId _)) = do
  anyValidObservedStrike <- tryFetchValidObservedStrikeByClass
  case anyValidObservedStrike of
    Nothing -> return [ ]
    Just maybeObservedStrike ->
      return [ (strikeE, maybeObservedStrike, guessesCount)]
  where
  tryFetchValidObservedStrikeByClass = do
    let
        anyStrikeClassContraintByFilter = maybe
          Nothing
          ( API.blockTimeStrikeFilterClass
          . fst
          . API.unFilterRequest
          ) mfilter
    case anyStrikeClassContraintByFilter of
      Nothing -> do
        let
            observedStrikeFilter = maybe
              [] ( API.buildFilter
                 . API.unFilterRequest
                 . API.mapFilter
                 ) mfilter
        anyObservedStrike <- selectFirst
          ( ( BlockTimeStrikeObservedStrike ==. strikeId
            )
          : observedStrikeFilter
          ) []
        case (observedStrikeFilter, anyObservedStrike) of
          ([], classDoNotConstraintExistenceOfObservedStrike) ->
            return (Just classDoNotConstraintExistenceOfObservedStrike)
          ( _nonEmptyObservedStrikeFilter: _ , Just observedBlockFitsFilter)->
            return (Just (Just observedBlockFitsFilter))
          (_observedStrikeFilterExistsButObservedStrikeMissing :_ , Nothing) ->
            return Nothing
      Just API.BlockTimeStrikeFilterClassGuessable->
        return (Just Nothing) -- strike has not been observed and strikeFilter should ensure, that it is in the future with proper guess threshold
      Just API.BlockTimeStrikeFilterClassOutcomeUnknown-> do
        return (Just Nothing) -- hasn't been observed
      Just API.BlockTimeStrikeFilterClassOutcomeKnown-> do
        -- now get possible observed data for strike
        anyObservedStrikeFitsOutcomeKnownClass <- selectFirst
          ( (BlockTimeStrikeObservedStrike ==. strikeId)
          : maybe [] ( API.buildFilter
                      . API.unFilterRequest
                      . API.mapFilter
                      ) mfilter
          )
          []
        case anyObservedStrikeFitsOutcomeKnownClass of
          Nothing -> return Nothing
          Just observedStrikeFitsOutcomeKnownClass ->
            return (Just (Just observedStrikeFitsOutcomeKnownClass)) -- had been observed
renderBlockTimeStrike
  :: ( Entity BlockTimeStrike
     , Maybe (Entity BlockTimeStrikeObserved)
     , Entity CalculatedBlockTimeStrikeGuessesCount
     )
  -> API.BlockTimeStrikeWithGuessesCount
renderBlockTimeStrike (Entity _ strike, mObserved, Entity _ guessesCount) =
  API.BlockTimeStrikeWithGuessesCount
    { blockTimeStrikeWithGuessesCountStrike = API.BlockTimeStrike
      { blockTimeStrikeObservedResult = fmap
        (\(Entity _ v)-> SlowFast.apiModel $ blockTimeStrikeObservedIsFast v)
        mObserved
      , blockTimeStrikeObservedBlockMediantime = fmap
        (\(Entity _ v)-> blockTimeStrikeObservedJudgementBlockMediantime v)
        mObserved
      , blockTimeStrikeObservedBlockHash = fmap
        (\(Entity _ v)-> blockTimeStrikeObservedJudgementBlockHash v)
        mObserved
      , blockTimeStrikeObservedBlockHeight = fmap
        (\(Entity _ v)-> blockTimeStrikeObservedJudgementBlockHeight v)
        mObserved
      , blockTimeStrikeBlock = blockTimeStrikeBlock strike
      , blockTimeStrikeStrikeMediantime =
        blockTimeStrikeStrikeMediantime strike
      , blockTimeStrikeCreationTime =
        blockTimeStrikeCreationTime strike
      }
    , blockTimeStrikeWithGuessesCountGuessesCount =
      fromIntegral ( calculatedBlockTimeStrikeGuessesCountGuessesCount guessesCount)
    }

