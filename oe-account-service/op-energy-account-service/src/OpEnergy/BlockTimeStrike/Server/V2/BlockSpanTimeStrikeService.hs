{-- | This module implements BlockTime strike service.
 -}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE EmptyDataDecls          #-}
{-# LANGUAGE GADTs                     #-}
module OpEnergy.BlockTimeStrike.Server.V2.BlockSpanTimeStrikeService
  ( createBlockSpanTimeStrikeFuture
  , newTipHandlerLoop
  , getBlockTimeStrikesPage
  , getBlockTimeStrike
  ) where

import           Servant (err400, err500, Handler)
import           Control.Monad.Trans.Reader ( ask, asks, ReaderT)
import           Control.Monad.Logger( logError, logInfo, NoLoggingT)
import           Control.Monad( when, forever)
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
                   ( runExceptT
                   , ExceptT (..)
                   , throwE
                   )
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
import           Data.OpEnergy.API.V1.Positive
                   ( naturalFromPositive
                   , fromPositive
                   , Positive
                   )
import qualified Data.OpEnergy.Account.API.V1.BlockTimeStrike            as API
import qualified Data.OpEnergy.Account.API.V1.PagingResult               as API
import qualified Data.OpEnergy.Account.API.V1.FilterRequest              as API
import qualified Data.OpEnergy.Account.API.V1.BlockTimeStrikeFilterClass as API
import           Data.OpEnergy.API.V1.Error(throwJSON)

import           OpEnergy.ExceptMaybe
                   ( exceptTMaybeT
                   , eitherLogThrowOrReturn
                   , runExceptPrefixT
                   )
import           OpEnergy.Error( eitherThrowJSON)
import           OpEnergy.PagingResult
import qualified OpEnergy.BlockTimeStrike.Server.V1.Class as BlockTime
import qualified OpEnergy.BlockTimeStrike.Server.V2.BlockSpanTimeScheduledStrikeCreation as BlockTimeScheduledStrikeCreation
import qualified OpEnergy.BlockTimeStrike.Server.V2.BlockSpanTimeStrikeObserve as Observe
import qualified OpEnergy.BlockTimeStrike.Server.V1.Context as Context
import qualified OpEnergy.BlockTimeStrike.Server.V2.BlockSpanTimeStrikeFilter as BlockTimeStrikeFilter
import qualified OpEnergy.BlockTimeStrike.Server.V2.DBModel as DB
import           OpEnergy.BlockTimeStrike.Server.V1.SlowFast
import           OpEnergy.Account.Server.V1.Config (Config(..))
import qualified OpEnergy.Account.Server.V1.Config  as Config
import           OpEnergy.Account.Server.V1.AccountService (mgetPersonByAccountToken)
import           OpEnergy.Account.Server.V1.Class (AppT, AppM, State(..), runLogging, profile, withDBTransaction )
import           OpEnergy.Account.Server.V1.Person

-- | O(ln accounts).
-- Tries to create future block time strike. Requires authenticated user and blockheight should be in the future
createBlockSpanTimeStrikeFuture
  :: API.AccountToken
  -> BlockHeight
  -> Positive Int
  -> Natural Int
  -> AppM ()
createBlockSpanTimeStrikeFuture token blockHeight spanSize strikeMediantime =
    let name = "createBlockSpanTimeStrikeFuture"
    in profile name $ eitherLogThrowOrReturn $ runExceptPrefixT name $ do
  configBlockTimeStrikeMinimumBlockAheadCurrentTip <- lift $ asks
    $ Config.configBlockTimeStrikeMinimumBlockAheadCurrentTip . config
  latestUnconfirmedBlockHeightV <- lift $ asks
    $ BlockTime.latestUnconfirmedBlockHeight . blockTimeState
  tip <- exceptTMaybeT "there is no current observed tip yet"
    $ liftIO $ TVar.readTVarIO latestUnconfirmedBlockHeightV
  when (tip > fromIntegral strikeMediantime)
    $ throwE "strikeMediantime is in the past, which is not expected"
  when ( tip + naturalFromPositive configBlockTimeStrikeMinimumBlockAheadCurrentTip
         > blockHeight
       )
    $ throwE "block height for new block time strike should be in the future \
             \+ minimum configBlockTimeStrikeMinimumBlockAheadCurrentTip"
  person <- exceptTMaybeT "person was not able to authenticate itself"
    $ mgetPersonByAccountToken token
  exceptTMaybeT "something went wrong"
    $ createBlockTimeStrikeEnsuredConditions person
  where
    createBlockTimeStrikeEnsuredConditions
      :: (MonadIO m, MonadMonitor m)
      => Entity Person
      -> AppT m (Maybe ())
    createBlockTimeStrikeEnsuredConditions _ = do
      nowUTC <- liftIO getCurrentTime
      let now = utcTimeToPOSIXSeconds nowUTC
      withDBTransaction "" $ insert_ $! DB.BlockSpanTimeStrike
        { DB.blockSpanTimeStrikeBlock = blockHeight
        , DB.blockSpanTimeStrikeMediantime = fromIntegral strikeMediantime
        , DB.blockSpanTimeStrikeCreationTime = now
        , DB.blockSpanTimeStrikeSpanSize = spanSize
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

-- | returns list of BlockTimeStrike records
getBlockTimeStrikesPage
  :: Maybe (Natural Int)
  -> Maybe (API.FilterRequest API.BlockTimeStrike API.BlockTimeStrikeFilter)
  -> AppM (API.PagingResult API.BlockTimeStrikeWithGuessesCount)
getBlockTimeStrikesPage mpage mfilterAPI = profile "getBlockTimeStrikesPage" $ do
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
    mfilter = fmap DB.coerceFilterRequestBlockSpanTimeStrike mfilterAPI
    sort = maybe
             Descend
             ( API.sortOrder
             . API.unFilterRequest
             )
             mfilter
    getBlockTimeStrikePast
      :: [Filter DB.BlockSpanTimeStrike]
      -> ReaderT
         State
         Handler
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
          DB.BlockSpanTimeStrikeId -- select strikes with given filter first
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
          DB.CalculatedBlockSpanTimeStrikeGuessesCountGuessesCount
          $  C.concatMapM (fetchStrikeByGuessesCount strikeFilter)
          .| C.map guessesCountAlreadyFetched
          .| C.concatMapM maybeFetchObservedStrike
          .| C.concatMapM unwrapGuessesCount
          .| C.map renderBlockTimeStrike
    guessesCountWillBeFetchedLater
      :: Entity DB.BlockSpanTimeStrike
      -> ( Maybe (Entity DB.CalculatedBlockSpanTimeStrikeGuessesCount)
         , Entity DB.BlockSpanTimeStrike
         )
    guessesCountWillBeFetchedLater strikeE = (Nothing, strikeE)
    guessesCountAlreadyFetched
      :: ( Entity DB.CalculatedBlockSpanTimeStrikeGuessesCount
         , Entity DB.BlockSpanTimeStrike
         )
      -> ( Maybe (Entity DB.CalculatedBlockSpanTimeStrikeGuessesCount)
         , Entity DB.BlockSpanTimeStrike
         )
    guessesCountAlreadyFetched (guessE, strikeE) = (Just guessE, strikeE)
    maybeFetchGuessesCount
      :: ( Entity DB.BlockSpanTimeStrike
         , Maybe (Entity DB.BlockSpanTimeStrikeObserved)
         , Maybe (Entity DB.CalculatedBlockSpanTimeStrikeGuessesCount)
         )
      -> (ReaderT SqlBackend (NoLoggingT (ResourceT IO)))
         [( Entity DB.BlockSpanTimeStrike
          , Maybe (Entity DB.BlockSpanTimeStrikeObserved)
          , Maybe (Entity DB.CalculatedBlockSpanTimeStrikeGuessesCount)
          )
         ]
    maybeFetchGuessesCount (strikeE@(Entity strikeId _), mObserved, mguessesCount) = do
      case mguessesCount of
        Just _ -> return [(strikeE, mObserved, mguessesCount)]
        Nothing -> do
          mguessesCount <- selectFirst
            [ DB.CalculatedBlockSpanTimeStrikeGuessesCountStrike ==. strikeId]
            []
          case mguessesCount of
            Just _ -> return [(strikeE, mObserved, mguessesCount)]
            Nothing -> do -- fallback mode, recount online, which maybe a bad thing to do here TODO decide if it should be removed
              return []
    fetchStrikeByGuessesCount
      :: [Filter DB.BlockSpanTimeStrike]
      -> Entity DB.CalculatedBlockSpanTimeStrikeGuessesCount
      -> (ReaderT SqlBackend (NoLoggingT (ResourceT IO)))
         [ ( Entity DB.CalculatedBlockSpanTimeStrikeGuessesCount
           , Entity DB.BlockSpanTimeStrike
           )
         ]
    fetchStrikeByGuessesCount strikeFilter guessE@(Entity _ guessesCount) = do
      mstrike <- selectFirst
        (( DB.BlockSpanTimeStrikeId
           ==. DB.calculatedBlockSpanTimeStrikeGuessesCountStrike guessesCount
         )
         :strikeFilter
        )
        []
      case mstrike of
        Just strikeE-> return [(guessE, strikeE)]
        Nothing -> return []
    maybeFetchObservedStrike
      :: ( Maybe (Entity DB.CalculatedBlockSpanTimeStrikeGuessesCount)
         , Entity DB.BlockSpanTimeStrike
         )
      -> (ReaderT SqlBackend (NoLoggingT (ResourceT IO)))
         [( Entity DB.BlockSpanTimeStrike
          , Maybe (Entity DB.BlockSpanTimeStrikeObserved)
          , Maybe (Entity DB.CalculatedBlockSpanTimeStrikeGuessesCount)
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
              ( ( DB.BlockSpanTimeStrikeObservedStrike ==. strikeId
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
              ( (DB.BlockSpanTimeStrikeObservedStrike ==. strikeId)
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
      :: ( Entity DB.BlockSpanTimeStrike
         , Maybe (Entity DB.BlockSpanTimeStrikeObserved)
         , Natural Int
         )
      -> API.BlockTimeStrikeWithGuessesCount
    renderBlockTimeStrike (Entity _ strike, mObserved, guessesCount) =
      API.BlockTimeStrikeWithGuessesCount
        { blockTimeStrikeWithGuessesCountStrike = API.BlockTimeStrike
          { blockTimeStrikeObservedResult = fmap
            (\(Entity _ v)-> apiModelSlowFast
              $ DB.blockSpanTimeStrikeObservedIsFast v
            )
            mObserved
          , blockTimeStrikeObservedBlockMediantime = fmap
            (\(Entity _ v)->
              DB.blockSpanTimeStrikeObservedJudgementBlockMediantime v
            )
            mObserved
          , blockTimeStrikeObservedBlockHash = fmap
            (\(Entity _ v)-> DB.blockSpanTimeStrikeObservedJudgementBlockHash v)
            mObserved
          , blockTimeStrikeObservedBlockHeight = fmap
            (\(Entity _ v)->
              DB.blockSpanTimeStrikeObservedJudgementBlockHeight v
            )
            mObserved
          , blockTimeStrikeBlock = DB.blockSpanTimeStrikeBlock strike
          , blockTimeStrikeStrikeMediantime =
            DB.blockSpanTimeStrikeMediantime strike
          , blockTimeStrikeCreationTime =
            DB.blockSpanTimeStrikeCreationTime strike
          }
        , blockTimeStrikeWithGuessesCountGuessesCount =
          fromIntegral guessesCount
        }
    unwrapGuessesCount
      :: ( Entity DB.BlockSpanTimeStrike
         , Maybe (Entity DB.BlockSpanTimeStrikeObserved)
         , Maybe (Entity DB.CalculatedBlockSpanTimeStrikeGuessesCount)
         )
      -> (ReaderT SqlBackend (NoLoggingT (ResourceT IO)))
         [( Entity DB.BlockSpanTimeStrike
          , Maybe (Entity DB.BlockSpanTimeStrikeObserved)
          , Natural Int
          )
         ]
    unwrapGuessesCount (strikeE, mObserved, mguessesCount) = do
      case mguessesCount of
        Nothing -> return []
        Just (Entity _ guessesCount) ->
          return [( strikeE
                  , mObserved
                  , DB.calculatedBlockSpanTimeStrikeGuessesCountGuessesCount guessesCount
                  )
                 ]


-- | returns BlockTimeStrike records
getBlockTimeStrike
  :: BlockHeight
  -> Natural Int
  -> AppM API.BlockTimeStrikeWithGuessesCount
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
            [ DB.BlockSpanTimeStrikeBlock ==. blockHeight
            , DB.BlockSpanTimeStrikeMediantime ==.
              fromIntegral strikeMediantime
            ]
            DB.BlockSpanTimeStrikeId
            (PageSize (fromPositive recordsPerReply + 1))
            Descend
            (Range Nothing Nothing)
          .| C.mapM getGuessesCountByBlockTimeStrike
          .| C.mapM maybeFetchObserved
          .| C.map renderBlockTimeStrikeWithGuessesCount
          .| C.head
    getGuessesCountByBlockTimeStrike
      :: Entity DB.BlockSpanTimeStrike
      -> ReaderT SqlBackend (NoLoggingT (ResourceT IO))
         (Entity DB.BlockSpanTimeStrike, Natural Int)
    getGuessesCountByBlockTimeStrike strikeE@(Entity strikeId _) = do
      mguessesCount <- selectFirst
        [ DB.CalculatedBlockSpanTimeStrikeGuessesCountStrike ==. strikeId]
        []
      let guessesCount = maybe
            0
            (\(Entity _ v) ->
              DB.calculatedBlockSpanTimeStrikeGuessesCountGuessesCount v
            )
            mguessesCount
      return (strikeE, guessesCount)
    maybeFetchObserved
      :: (Entity DB.BlockSpanTimeStrike, Natural Int)
      -> ReaderT SqlBackend (NoLoggingT (ResourceT IO))
         ( Entity DB.BlockSpanTimeStrike
         , Natural Int
         , Maybe (Entity DB.BlockSpanTimeStrikeObserved)
         )
    maybeFetchObserved (strikeE@(Entity strikeId _), guessesCount) = do
      mObserved <- selectFirst
        [ DB.BlockSpanTimeStrikeObservedStrike ==. strikeId ]
        []
      return (strikeE, guessesCount, mObserved)
    renderBlockTimeStrikeWithGuessesCount
      :: ( Entity DB.BlockSpanTimeStrike
         , Natural Int
         , Maybe (Entity DB.BlockSpanTimeStrikeObserved)
         )
      -> API.BlockTimeStrikeWithGuessesCount
    renderBlockTimeStrikeWithGuessesCount
        (Entity _ strike, guessesCount, mObserved) =
      API.BlockTimeStrikeWithGuessesCount
        { blockTimeStrikeWithGuessesCountStrike = API.BlockTimeStrike
          { blockTimeStrikeObservedResult = fmap
            (\(Entity _ v)-> apiModelSlowFast
              $ DB.blockSpanTimeStrikeObservedIsFast v
            )
            mObserved
          , blockTimeStrikeObservedBlockMediantime = fmap
            (\(Entity _ v)->
              DB.blockSpanTimeStrikeObservedJudgementBlockMediantime v
            )
            mObserved
          , blockTimeStrikeObservedBlockHash = fmap
            (\(Entity _ v)->
              DB.blockSpanTimeStrikeObservedJudgementBlockHash v
            )
            mObserved
          , blockTimeStrikeObservedBlockHeight = fmap
            (\(Entity _ v)->
              DB.blockSpanTimeStrikeObservedJudgementBlockHeight v
            )
            mObserved
          , blockTimeStrikeBlock = DB.blockSpanTimeStrikeBlock strike
          , blockTimeStrikeStrikeMediantime =
            DB.blockSpanTimeStrikeMediantime strike
          , blockTimeStrikeCreationTime =
            DB.blockSpanTimeStrikeCreationTime strike
          }
        , blockTimeStrikeWithGuessesCountGuessesCount =
          fromIntegral guessesCount
        }

