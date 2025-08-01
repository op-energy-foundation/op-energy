{-- | This module implements BlockTime strike service.
 -}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE EmptyDataDecls          #-}
{-# LANGUAGE GADTs                     #-}
module OpEnergy.BlockTimeStrike.Server.V1.BlockTimeStrikeService
  ( createBlockTimeStrikeFuture
  , newTipHandlerLoop
  , getBlockTimeStrikesPage
  , getBlockTimeStrike
  ) where

import           Servant (err400, err500, Handler)
import           Control.Monad.Trans.Reader ( ask, asks, ReaderT)
import           Control.Monad.Logger( logError, logInfo, NoLoggingT)
import           Control.Monad(forever)
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
import           Control.Monad.Trans.Except( runExceptT, ExceptT (..))
import           Control.Monad.Trans.Resource( ResourceT)
import           Data.Maybe( fromMaybe)

import           Database.Persist
import           Database.Persist.Sql
import           Database.Persist.Pagination
import           Prometheus(MonadMonitor)


import           Data.Text.Show (tshow)
import           Data.OpEnergy.Account.API.V1.Account
import           Data.OpEnergy.API.V1.Block
import           Data.OpEnergy.API.V1.Natural
import           Data.OpEnergy.API.V1.Positive( naturalFromPositive, fromPositive)
import           Data.OpEnergy.Account.API.V1.BlockTimeStrike
import           Data.OpEnergy.Account.API.V1.BlockTimeStrikeGuess
import           Data.OpEnergy.Account.API.V1.BlockTimeStrikePublic
import           Data.OpEnergy.Account.API.V1.PagingResult
import           Data.OpEnergy.Account.API.V1.FilterRequest
import           Data.OpEnergy.Account.API.V1.BlockTimeStrikeFilterClass
import           Data.OpEnergy.API.V1.Error(throwJSON)
import           OpEnergy.Account.Server.V1.Config (Config(..))
import           OpEnergy.Account.Server.V1.Class (AppT, AppM, State(..), runLogging, profile, withDBTransaction )
import qualified OpEnergy.BlockTimeStrike.Server.V1.Class as BlockTime
import           OpEnergy.Account.Server.V1.AccountService (mgetPersonByAccountToken)
import qualified OpEnergy.BlockTimeStrike.Server.V1.BlockTimeScheduledStrikeCreation as BlockTimeScheduledStrikeCreation
import           OpEnergy.PagingResult
import qualified OpEnergy.BlockTimeStrike.Server.V1.BlockTimeStrikeObserve as Observe
import qualified OpEnergy.BlockTimeStrike.Server.V1.Context as Context
import           OpEnergy.ExceptMaybe(exceptTMaybeT)
import           OpEnergy.Error( eitherThrowJSON)
import qualified OpEnergy.BlockTimeStrike.Server.V1.BlockTimeStrikeFilter as BlockTimeStrikeFilter

-- | O(ln accounts).
-- Tries to create future block time strike. Requires authenticated user and blockheight should be in the future
createBlockTimeStrikeFuture :: AccountToken-> BlockHeight-> Natural Int-> AppM ()
createBlockTimeStrikeFuture token blockHeight strikeMediantime = profile "createBlockTimeStrike" $ do
  State{ config = Config{ configBlockTimeStrikeMinimumBlockAheadCurrentTip = configBlockTimeStrikeMinimumBlockAheadCurrentTip}
       , blockTimeState = BlockTime.State{ latestUnconfirmedBlockHeight = latestUnconfirmedBlockHeightV }
       } <- ask
  mlatestUnconfirmedBlockHeight <- liftIO $ TVar.readTVarIO latestUnconfirmedBlockHeightV
  case mlatestUnconfirmedBlockHeight of
    Nothing -> do
      let err = "ERROR: createBlockTimeStrike: there is no current observed tip yet"
      runLogging $ $(logError) err
      throwJSON err400 err
    Just tip
      | tip > fromIntegral strikeMediantime -> do
        let err = "ERROR: strikeMediantime is in the past, which is not expected"
        runLogging $ $(logError) err
        throwJSON err400 err
    Just tip
      | tip + naturalFromPositive configBlockTimeStrikeMinimumBlockAheadCurrentTip > blockHeight -> do
        let msg = "ERROR: createBlockTimeStrike: block height for new block time strike should be in the future + minimum configBlockTimeStrikeMinimumBlockAheadCurrentTip"
        runLogging $ $(logError) msg
        throwJSON err400 msg
    _ -> do
      mperson <- mgetPersonByAccountToken token
      case mperson of
        Nothing -> do
          let err = "ERROR: createBlockTimeStrike: person was not able to authenticate itself"
          runLogging $ $(logError) err
          throwJSON err400 err
        Just person -> do
          mret <- createBlockTimeStrikeEnsuredConditions person
          case mret of
            Just ret -> return ret
            Nothing -> do
              throwJSON err500 (("something went wrong")::Text)
  where
    createBlockTimeStrikeEnsuredConditions :: (MonadIO m, MonadMonitor m) => (Entity Person) -> AppT m (Maybe ())
    createBlockTimeStrikeEnsuredConditions _ = do
      nowUTC <- liftIO getCurrentTime
      let now = utcTimeToPOSIXSeconds nowUTC
      withDBTransaction "" $ insert_ $! BlockTimeStrike
        { blockTimeStrikeBlock = blockHeight
        , blockTimeStrikeStrikeMediantime = fromIntegral strikeMediantime
        , blockTimeStrikeCreationTime = now
        }

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

-- | returns list of BlockTimeStrikePublic records
getBlockTimeStrikesPage
  :: Maybe (Natural Int)
  -> Maybe (FilterRequest BlockTimeStrike BlockTimeStrikeFilter)
  -> AppM (PagingResult BlockTimeStrikeWithGuessesCountPublic)
getBlockTimeStrikesPage mpage mfilter = profile "getBlockTimeStrikesPage" $ do
  latestUnconfirmedBlockHeightV <- asks (BlockTime.latestUnconfirmedBlockHeight . blockTimeState)
  configBlockTimeStrikeGuessMinimumBlockAheadCurrentTip <- asks (configBlockTimeStrikeGuessMinimumBlockAheadCurrentTip . config)
  latestConfirmedBlockV <- asks (BlockTime.latestConfirmedBlock . blockTimeState)
  eitherThrowJSON
    (\reason-> do
      let msg = "getBlockTimeStrikesPage: " <> reason
      runLogging $ $(logError) msg
      return (err500, msg)
    )
    $ runExceptT $ do
      (latestUnconfirmedBlockHeight, latestConfirmedBlock) <-
        ExceptT $ liftIO $ STM.atomically $ runExceptT $ (,)
          <$> (exceptTMaybeT "latest unconfirmed block hasn't been received yet"
              $ TVar.readTVar latestUnconfirmedBlockHeightV
              )
          <*> (exceptTMaybeT "latest confirmed block hasn't been received yet"
              $ TVar.readTVar latestConfirmedBlockV
              )
      let
          staticPartFilter =  (maybe [] (buildFilter . unFilterRequest . mapFilter) mfilter)
          strikeFilter =
            BlockTimeStrikeFilter.buildFilterByClass
              (maybe Nothing (blockTimeStrikeFilterClass . fst . unFilterRequest) mfilter)
              latestUnconfirmedBlockHeight
              latestConfirmedBlock
              configBlockTimeStrikeGuessMinimumBlockAheadCurrentTip
            ++ staticPartFilter
      exceptTMaybeT "getBlockTimeStrikePast failed"
        $ getBlockTimeStrikePast strikeFilter
  where
    sort = maybe Descend (sortOrder . unFilterRequest) mfilter
    getBlockTimeStrikePast
      :: [Filter BlockTimeStrike]
      -> ReaderT
         State
         Handler
         (Maybe (PagingResult BlockTimeStrikeWithGuessesCountPublic))
    getBlockTimeStrikePast strikeFilter = do
      recordsPerReply <- asks (configRecordsPerReply . config)
      let
          linesPerPage = maybe
            recordsPerReply
            ( fromMaybe recordsPerReply
            . blockTimeStrikeFilterLinesPerPage
            . fst
            . unFilterRequest
            )
            mfilter
      let
          eGuessesCount = case maybe
                StrikeSortOrderDescend
                ( fromMaybe StrikeSortOrderDescend
                . blockTimeStrikeFilterSort
                . fst
                . unFilterRequest
                )
                mfilter
              of
            StrikeSortOrderAscend -> SortByGuessesCountNotNeeded
            StrikeSortOrderDescend -> SortByGuessesCountNotNeeded
            StrikeSortOrderAscendGuessesCount ->  SortByGuessesCountNeeded
            StrikeSortOrderDescendGuessesCount -> SortByGuessesCountNeeded
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
          .| C.map renderBlockTimeStrikePublic
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
          .| C.map renderBlockTimeStrikePublic
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
              ( blockTimeStrikeFilterClass
              . fst
              . unFilterRequest
              ) mfilter
        case anyStrikeClassContraintByFilter of
          Nothing -> do
            let
                observedStrikeFilter = maybe
                  [] ( buildFilter
                     . unFilterRequest
                     . mapFilter
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
          Just BlockTimeStrikeFilterClassGuessable->
            return (Just Nothing) -- strike has not been observed and strikeFilter should ensure, that it is in the future with proper guess threshold
          Just BlockTimeStrikeFilterClassOutcomeUnknown-> do
            return (Just Nothing) -- hasn't been observed
          Just BlockTimeStrikeFilterClassOutcomeKnown-> do
            -- now get possible observed data for strike
            anyObservedStrikeFitsOutcomeKnownClass <- selectFirst
              ( (BlockTimeStrikeObservedStrike ==. strikeId)
              : maybe [] ( buildFilter
                          . unFilterRequest
                          . mapFilter
                          ) mfilter
              )
              []
            case anyObservedStrikeFitsOutcomeKnownClass of
              Nothing -> return Nothing
              Just observedStrikeFitsOutcomeKnownClass ->
                return (Just (Just observedStrikeFitsOutcomeKnownClass)) -- had been observed
    renderBlockTimeStrikePublic
      :: ( Entity BlockTimeStrike
         , Maybe (Entity BlockTimeStrikeObserved)
         , Natural Int
         )
      -> BlockTimeStrikeWithGuessesCountPublic
    renderBlockTimeStrikePublic (Entity _ strike, mObserved, guessesCount) =
      BlockTimeStrikeWithGuessesCountPublic
        { blockTimeStrikeWithGuessesCountPublicStrike = BlockTimeStrikePublic
          { blockTimeStrikePublicObservedResult = fmap
            (\(Entity _ v)-> blockTimeStrikeObservedIsFast v)
            mObserved
          , blockTimeStrikePublicObservedBlockMediantime = fmap
            (\(Entity _ v)-> blockTimeStrikeObservedJudgementBlockMediantime v)
            mObserved
          , blockTimeStrikePublicObservedBlockHash = fmap
            (\(Entity _ v)-> blockTimeStrikeObservedJudgementBlockHash v)
            mObserved
          , blockTimeStrikePublicObservedBlockHeight = fmap
            (\(Entity _ v)-> blockTimeStrikeObservedJudgementBlockHeight v)
            mObserved
          , blockTimeStrikePublicBlock = blockTimeStrikeBlock strike
          , blockTimeStrikePublicStrikeMediantime =
            blockTimeStrikeStrikeMediantime strike
          , blockTimeStrikePublicCreationTime =
            blockTimeStrikeCreationTime strike
          }
        , blockTimeStrikeWithGuessesCountPublicGuessesCount =
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


-- | returns BlockTimeStrikePublic records
getBlockTimeStrike
  :: BlockHeight
  -> Natural Int
  -> AppM BlockTimeStrikeWithGuessesCountPublic
getBlockTimeStrike blockHeight strikeMediantime = profile "getBlockTimeStrike" $ do
  mret <- actualGetBlockTimeStrike
  case mret of
    Nothing -> do
      throwJSON err500 ("something went wrong"::Text)
    Just Nothing -> do
      throwJSON err400 ("strike not found"::Text)
    Just (Just ret) -> return ret
  where
    actualGetBlockTimeStrike = do
      recordsPerReply <- asks (configRecordsPerReply . config)
      withDBTransaction "" $ do
        C.runConduit
          $ streamEntities
            [ BlockTimeStrikeBlock ==. blockHeight, BlockTimeStrikeStrikeMediantime ==. fromIntegral strikeMediantime ]
            BlockTimeStrikeId
            (PageSize (fromPositive recordsPerReply + 1))
            Descend
            (Range Nothing Nothing)
          .| C.mapM getGuessesCountByBlockTimeStrike
          .| C.mapM maybeFetchObserved
          .| C.map renderBlockTimeStrikeWithGuessesCountPublic
          .| C.head
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
    renderBlockTimeStrikeWithGuessesCountPublic
      :: (Entity BlockTimeStrike, Natural Int, Maybe (Entity BlockTimeStrikeObserved))
      -> BlockTimeStrikeWithGuessesCountPublic
    renderBlockTimeStrikeWithGuessesCountPublic
        (Entity _ strike, guessesCount, mObserved) =
      BlockTimeStrikeWithGuessesCountPublic
        { blockTimeStrikeWithGuessesCountPublicStrike = BlockTimeStrikePublic
          { blockTimeStrikePublicObservedResult = fmap
            (\(Entity _ v)-> blockTimeStrikeObservedIsFast v)
            mObserved
          , blockTimeStrikePublicObservedBlockMediantime = fmap
            (\(Entity _ v)-> blockTimeStrikeObservedJudgementBlockMediantime v)
            mObserved
          , blockTimeStrikePublicObservedBlockHash = fmap
            (\(Entity _ v)-> blockTimeStrikeObservedJudgementBlockHash v)
            mObserved
          , blockTimeStrikePublicObservedBlockHeight = fmap
            (\(Entity _ v)-> blockTimeStrikeObservedJudgementBlockHeight v)
            mObserved
          , blockTimeStrikePublicBlock = blockTimeStrikeBlock strike
          , blockTimeStrikePublicStrikeMediantime =
            blockTimeStrikeStrikeMediantime strike
          , blockTimeStrikePublicCreationTime =
            blockTimeStrikeCreationTime strike
          }
        , blockTimeStrikeWithGuessesCountPublicGuessesCount =
          fromIntegral guessesCount
        }

