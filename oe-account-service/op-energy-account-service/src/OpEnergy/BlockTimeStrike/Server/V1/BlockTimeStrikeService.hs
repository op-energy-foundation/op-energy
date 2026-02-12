{-- | This module implements BlockTime strike service.
 -}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE EmptyDataDecls          #-}
{-# LANGUAGE GADTs                     #-}
module OpEnergy.BlockTimeStrike.Server.V1.BlockTimeStrikeService
  ( createBlockTimeStrikeFutureHandler
  , createBlockTimeStrikeFuture
  , newTipHandlerLoop
  , getBlockTimeStrikesPage
  , getBlockTimeStrikesPageHandler
  , getBlockTimeStrike
  , getBlockTimeStrikeHandler
  ) where

import           Servant ( err500)
import           Control.Monad.Trans.Reader ( ask, asks, ReaderT)
import           Control.Monad.Logger( logError, logInfo, NoLoggingT)
import           Control.Monad(forever, when, void)
import           Data.Time.Clock(getCurrentTime)
import           Data.Time.Clock.POSIX(utcTimeToPOSIXSeconds)
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TVar as TVar
import qualified Control.Concurrent.MVar as MVar
import           Data.Text (Text)
import           Data.Conduit( (.|) )
import qualified Data.Conduit as C
import qualified Data.Conduit.List as C
import           Control.Monad.Trans
import           Control.Monad.Trans.Except
                   ( runExceptT, ExceptT (..), throwE)
import           Control.Monad.Trans.Resource( ResourceT)
import           Data.Maybe( fromMaybe)

import           Database.Persist
import           Database.Persist.Sql
import           Database.Persist.Pagination
import           Prometheus(MonadMonitor)


import           Data.Text.Show (tshow)
import qualified Data.OpEnergy.Account.API.V1.Account as API
import           Data.OpEnergy.API.V1.Block
import           Data.OpEnergy.API.V1.Natural
import           Data.OpEnergy.API.V1.Positive( naturalFromPositive, fromPositive)
import qualified Data.OpEnergy.Account.API.V1.BlockTimeStrike            as API
import qualified Data.OpEnergy.Account.API.V1.PagingResult               as API
import qualified Data.OpEnergy.Account.API.V1.FilterRequest              as API
import qualified Data.OpEnergy.Account.API.V1.BlockTimeStrikeFilterClass as API

import           OpEnergy.ExceptMaybe(exceptTMaybeT)
import           OpEnergy.Error( eitherThrowJSON, runExceptPrefixT)
import           OpEnergy.PagingResult
import qualified OpEnergy.BlockTimeStrike.Server.V1.Class as BlockTime
import qualified OpEnergy.BlockTimeStrike.Server.V1.BlockTimeScheduledStrikeCreation as BlockTimeScheduledStrikeCreation
import qualified OpEnergy.BlockTimeStrike.Server.V1.BlockTimeStrikeObserve as Observe
import qualified OpEnergy.BlockTimeStrike.Server.V1.Context as Context
import qualified OpEnergy.BlockTimeStrike.Server.V1.BlockTimeStrikeFilter as BlockTimeStrikeFilter
import           OpEnergy.BlockTimeStrike.Server.V1.BlockTimeStrike
import           OpEnergy.BlockTimeStrike.Server.V1.BlockTimeStrikeGuess
import qualified OpEnergy.BlockTimeStrike.Server.V1.SlowFast as SlowFast
import           OpEnergy.Account.Server.V1.Config
                 as Config(Config(..))
import           OpEnergy.Account.Server.V1.AccountService (mgetPersonByAccountToken)
import           OpEnergy.Account.Server.V1.Class (AppT, AppM, State(..), runLogging, profile, withDBTransaction )
import           OpEnergy.Account.Server.V1.Person

-- | O(ln accounts).
-- Tries to create future block time strike. Requires authenticated user and blockheight should be in the future
createBlockTimeStrikeFutureHandler
  :: API.AccountToken
  -> BlockHeight
  -> Natural Int
  -> AppM ()
createBlockTimeStrikeFutureHandler token blockHeight strikeMediantime =
    let name = "createBlockTimeStrikeFuture"
    in profile name $ do
  eitherThrowJSON
    (\reason-> do
      callstack <- asks callStack
      runLogging $ $(logError) $ callstack <> ": " <> reason
      return (err500, reason)
    )
    $ runExceptPrefixT name $ do
    void $ ExceptT $ createBlockTimeStrikeFuture token blockHeight
      strikeMediantime

createBlockTimeStrikeFuture
  :: API.AccountToken
  -> BlockHeight
  -> Natural Int
  -> AppM (Either Text BlockTimeStrike)
createBlockTimeStrikeFuture token blockHeight strikeMediantime =
    let name = "createBlockTimeStrikeFuture"
    in profile name $ runExceptPrefixT name $ do
  configBlockTimeStrikeMinimumBlockAheadCurrentTip <- lift $ asks
    $ Config.configBlockTimeStrikeMinimumBlockAheadCurrentTip . config
  latestUnconfirmedBlockHeightV <- lift $ asks (BlockTime.latestUnconfirmedBlockHeight . blockTimeState)
  latestConfirmedBlockV <- lift $ asks
    $ BlockTime.latestConfirmedBlock . blockTimeState
  (tip, latestConfirmedBlock) <-
    ExceptT $ liftIO $ STM.atomically $ runExceptT $ (,)
      <$> (exceptTMaybeT "latest unconfirmed block hasn't been received yet"
          $ TVar.readTVar latestUnconfirmedBlockHeightV
          )
      <*> (exceptTMaybeT "latest confirmed block hasn't been received yet"
          $ TVar.readTVar latestConfirmedBlockV
          )
  when (blockHeaderMediantime latestConfirmedBlock >= fromIntegral strikeMediantime) $
    throwE "strikeMediantime is in the past, which is not expected"
  when ( tip + naturalFromPositive configBlockTimeStrikeMinimumBlockAheadCurrentTip
         > blockHeight
       ) $ throwE "block height for new block time strike should be in the \
           \future + minimum configBlockTimeStrikeMinimumBlockAheadCurrentTip"
  person <- exceptTMaybeT "person was not able to authenticate itself"
    $ mgetPersonByAccountToken token
  exceptTMaybeT "something went wrong"
    $ createBlockTimeStrikeEnsuredConditions person
  where
    createBlockTimeStrikeEnsuredConditions
      :: (MonadIO m, MonadMonitor m)
      => Entity Person
      -> AppT m (Maybe BlockTimeStrike)
    createBlockTimeStrikeEnsuredConditions _ = do
      nowUTC <- liftIO getCurrentTime
      let
          now = utcTimeToPOSIXSeconds nowUTC
          record = BlockTimeStrike
            { blockTimeStrikeBlock = blockHeight
            , blockTimeStrikeStrikeMediantime = fromIntegral strikeMediantime
            , blockTimeStrikeCreationTime = now
            }
      withDBTransaction "" $ do
        insert_ record
        return record

-- | this function is an entry point for a process, that creates blocktime past strikes when such block is being
-- confirmed. After BlockTimeStrikePast creation, it will add record to the BlockTimeStrikeFutureObservedBlock table
-- in order to notify hndlers in the chain (currently only guess game), which should move appropriate references
-- to such blocktime future strike into past strikes. Then, those subsequent handlers should create
-- BlockTimeStrikeFutureReadyToRemove record, which should be handled by archiveFutureStrikesLoop function
newTipHandlerLoop :: (MonadIO m, MonadMonitor m) => AppT m ()
newTipHandlerLoop = forever $ do
  State{ blockTimeState = BlockTime.State
         { blockTimeStrikeConfirmedTip = confirmedTipV
         }
       } <- ask
  -- get new confirmed tip notification from upstream handler
  confirmedTip <- liftIO $ MVar.takeMVar confirmedTipV
  runLogging $ $(logInfo) $ "BlockTimeStrikeService: tipHandler: received new confirmed tip height: " <> (tshow $ blockHeaderHeight confirmedTip)
  -- find out any future strikes <= new confirmed tip
  profile "newTipHandlerIteration" $ do
    Observe.withLeastUnobservedConfirmedBlock confirmedTip $ \leastUnobservedConfirmedBlock-> do
      Observe.observeStrikes leastUnobservedConfirmedBlock
      BlockTimeScheduledStrikeCreation.maybeCreateStrikes $! Context.unContext leastUnobservedConfirmedBlock

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
      runLogging $ $(logError) $ callstack <> ": " <> reason
      return (err500, reason)
    )
    $ runExceptPrefixT name $ ExceptT $ getBlockTimeStrikesPage mpage mfilterAPI

getBlockTimeStrikesPage
  :: ( MonadIO m
     , MonadMonitor m
     )
  => Maybe (Natural Int)
  -> Maybe (API.FilterRequest API.BlockTimeStrike API.BlockTimeStrikeFilter)
  -> AppT m (Either Text (API.PagingResult API.BlockTimeStrikeWithGuessesCount))
getBlockTimeStrikesPage mpage mfilterAPI =
    let name = "getBlockTimeStrikesPage"
    in profile name $ runExceptPrefixT name $ do
  latestUnconfirmedBlockHeightV <- lift $ asks (BlockTime.latestUnconfirmedBlockHeight . blockTimeState)
  configBlockTimeStrikeGuessMinimumBlockAheadCurrentTip <- lift $ asks (configBlockTimeStrikeGuessMinimumBlockAheadCurrentTip . config)
  latestConfirmedBlockV <- lift $ asks (BlockTime.latestConfirmedBlock . blockTimeState)
  (latestUnconfirmedBlockHeight, latestConfirmedBlock) <-
    ExceptT $ liftIO $ STM.atomically $ runExceptT $ (,)
      <$> (exceptTMaybeT "latest unconfirmed block hasn't been received yet"
          $ TVar.readTVar latestUnconfirmedBlockHeightV
          )
      <*> (exceptTMaybeT "latest confirmed block hasn't been received yet"
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
  exceptTMaybeT "getBlockTimeStrikePast failed"
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
          $  C.map guessesCountWillBeFetchedLater
          .| C.concatMapM maybeFetchObservedStrike
          .| C.concatMapM maybeFetchGuessesCount
          .| C.concatMapM unwrapGuessesCount
          .| C.map renderBlockTimeStrike
        SortByGuessesCountNeeded -> pagingResult
          mpage
          linesPerPage
          []
          sort
          CalculatedBlockTimeStrikeGuessesCountGuessesCount
          $  C.concatMapM (fetchStrikeByGuessesCount strikeFilter)
          .| C.map guessesCountAlreadyFetched
          .| C.concatMapM maybeFetchObservedStrike
          .| C.concatMapM unwrapGuessesCount
          .| C.map renderBlockTimeStrike
    guessesCountWillBeFetchedLater
      :: Entity BlockTimeStrike
      -> ( Maybe (Entity CalculatedBlockTimeStrikeGuessesCount)
         , Entity BlockTimeStrike
         )
    guessesCountWillBeFetchedLater strikeE = (Nothing, strikeE)
    guessesCountAlreadyFetched
      :: ( Entity CalculatedBlockTimeStrikeGuessesCount
         , Entity BlockTimeStrike
         )
      -> ( Maybe (Entity CalculatedBlockTimeStrikeGuessesCount)
         , Entity BlockTimeStrike
         )
    guessesCountAlreadyFetched (guessE, strikeE) = (Just guessE, strikeE)
    maybeFetchGuessesCount
      :: ( Entity BlockTimeStrike
         , Maybe (Entity BlockTimeStrikeObserved)
         , Maybe (Entity CalculatedBlockTimeStrikeGuessesCount)
         )
      -> (ReaderT SqlBackend (NoLoggingT (ResourceT IO)))
         [( Entity BlockTimeStrike
          , Maybe (Entity BlockTimeStrikeObserved)
          , Maybe (Entity CalculatedBlockTimeStrikeGuessesCount)
          )
         ]
    maybeFetchGuessesCount (strikeE@(Entity strikeId _), mObserved, mguessesCount) = do
      case mguessesCount of
        Just _ -> return [(strikeE, mObserved, mguessesCount)]
        Nothing -> do
          mguessesCount <- selectFirst
            [ CalculatedBlockTimeStrikeGuessesCountStrike ==. strikeId]
            []
          case mguessesCount of
            Just _ -> return [(strikeE, mObserved, mguessesCount)]
            Nothing -> do -- fallback mode, recount online, which maybe a bad thing to do here TODO decide if it should be removed
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
      :: ( Maybe (Entity CalculatedBlockTimeStrikeGuessesCount)
         , Entity BlockTimeStrike
         )
      -> (ReaderT SqlBackend (NoLoggingT (ResourceT IO)))
         [( Entity BlockTimeStrike
          , Maybe (Entity BlockTimeStrikeObserved)
          , Maybe (Entity CalculatedBlockTimeStrikeGuessesCount)
          )
         ]
    maybeFetchObservedStrike (mguessesCount, strikeE@(Entity strikeId _)) = do
      anyValidObservedStrike <- tryFetchValidObservedStrikeByClass
      case anyValidObservedStrike of
        Nothing -> return [ ]
        Just maybeObservedStrike ->
          return [ (strikeE, maybeObservedStrike, mguessesCount)]
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
         , Natural Int
         )
      -> API.BlockTimeStrikeWithGuessesCount
    renderBlockTimeStrike (Entity _ strike, mObserved, guessesCount) =
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
          fromIntegral guessesCount
        }
    unwrapGuessesCount
      :: ( Entity BlockTimeStrike
         , Maybe (Entity BlockTimeStrikeObserved)
         , Maybe (Entity CalculatedBlockTimeStrikeGuessesCount)
         )
      -> (ReaderT SqlBackend (NoLoggingT (ResourceT IO)))
         [( Entity BlockTimeStrike
          , Maybe (Entity BlockTimeStrikeObserved)
          , Natural Int
          )
         ]
    unwrapGuessesCount (strikeE, mObserved, mguessesCount) = do
      case mguessesCount of
        Nothing -> return []
        Just (Entity _ guessesCount) ->
          return [( strikeE
                  , mObserved
                  , calculatedBlockTimeStrikeGuessesCountGuessesCount guessesCount
                  )
                 ]


getBlockTimeStrikeHandler
  :: BlockHeight
  -> Natural Int
  -> AppM API.BlockTimeStrikeWithGuessesCount
getBlockTimeStrikeHandler blockHeight strikeMediantime =
    let name = "V1.BlockTimeStrikeService.getBlockTimeStrikeHandler"
    in profile name $ eitherThrowJSON
  (\reason-> do
    callstack <- asks callStack
    runLogging $ $(logError) $ callstack <> ": " <> reason
    return (err500, reason)
  )
  $ getBlockTimeStrike blockHeight strikeMediantime

-- | returns BlockTimeStrike records
getBlockTimeStrike
  :: BlockHeight
  -> Natural Int
  -> AppM (Either Text API.BlockTimeStrikeWithGuessesCount)
getBlockTimeStrike blockHeight strikeMediantime =
    let name = "getBlockTimeStrike"
    in profile name $ runExceptPrefixT name $ do
  recordsPerReply <- lift $ asks (configRecordsPerReply . config)
  mStrike <- exceptTMaybeT "something went wrong"
    $ withDBTransaction "" $ do
        C.runConduit
          $ streamEntities
            [ BlockTimeStrikeBlock ==. blockHeight, BlockTimeStrikeStrikeMediantime ==. fromIntegral strikeMediantime ]
            BlockTimeStrikeId
            (PageSize (fromPositive recordsPerReply + 1))
            Descend
            (Range Nothing Nothing)
          .| C.mapM getGuessesCountByBlockTimeStrike
          .| C.mapM maybeFetchObserved
          .| C.map renderBlockTimeStrikeWithGuessesCount
          .| C.head
  exceptTMaybeT "strike not found" $ return mStrike
  where
    getGuessesCountByBlockTimeStrike
      :: Entity BlockTimeStrike
      -> ReaderT SqlBackend (NoLoggingT (ResourceT IO))
         (Entity BlockTimeStrike, Natural Int)
    getGuessesCountByBlockTimeStrike strikeE@(Entity strikeId _) = do
      mguessesCount <- selectFirst
        [ CalculatedBlockTimeStrikeGuessesCountStrike ==. strikeId]
        []
      let guessesCount = maybe
            0
            (\(Entity _ v) -> calculatedBlockTimeStrikeGuessesCountGuessesCount v)
            mguessesCount
      return (strikeE, guessesCount)
    maybeFetchObserved
      :: (Entity BlockTimeStrike, Natural Int)
      -> ReaderT SqlBackend (NoLoggingT (ResourceT IO))
         (Entity BlockTimeStrike, Natural Int, Maybe (Entity BlockTimeStrikeObserved))
    maybeFetchObserved (strikeE@(Entity strikeId _), guessesCount) = do
      mObserved <- selectFirst
        [ BlockTimeStrikeObservedStrike ==. strikeId ]
        []
      return (strikeE, guessesCount, mObserved)
    renderBlockTimeStrikeWithGuessesCount
      :: (Entity BlockTimeStrike, Natural Int, Maybe (Entity BlockTimeStrikeObserved))
      -> API.BlockTimeStrikeWithGuessesCount
    renderBlockTimeStrikeWithGuessesCount
        (Entity _ strike, guessesCount, mObserved) =
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
          fromIntegral guessesCount
        }

