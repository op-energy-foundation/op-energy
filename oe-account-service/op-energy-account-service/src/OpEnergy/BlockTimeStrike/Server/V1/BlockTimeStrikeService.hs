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

import           Servant (err400, err500)
import           Control.Monad.Trans.Reader ( ask, asks)
import           Control.Monad.Logger( logError, logInfo)
import           Control.Monad(forever )
import           Data.Time.Clock(getCurrentTime)
import           Data.Time.Clock.POSIX(utcTimeToPOSIXSeconds)
import qualified Control.Concurrent.STM.TVar as TVar
import qualified Control.Concurrent.MVar as MVar
import           Data.Text (Text)
import           Data.Conduit( (.|) )
import qualified Data.Conduit as C
import           Control.Monad.Trans

import           Database.Persist
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
import           OpEnergy.Account.Server.V1.Class (AppT, AppM, State(..), runLogging, profile, withDBTransaction, withDBNOTransactionROUnsafe )
import qualified OpEnergy.BlockTimeStrike.Server.V1.Class as BlockTime
import           OpEnergy.Account.Server.V1.AccountService (mgetPersonByAccountToken)
import qualified OpEnergy.BlockTimeStrike.Server.V1.BlockTimeScheduledStrikeCreation as BlockTimeScheduledStrikeCreation
import           OpEnergy.PagingResult
import qualified OpEnergy.BlockTimeStrike.Server.V1.BlockTimeStrikeObserve as Observe
import qualified OpEnergy.BlockTimeStrike.Server.V1.Context as Context

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

-- | returns list of BlockTimeStrikePublic records
getBlockTimeStrikesPage
  :: Maybe (Natural Int)
  -> Maybe (FilterRequest BlockTimeStrike BlockTimeStrikeFilter)
  -> AppM (PagingResult BlockTimeStrikeWithGuessesCountPublic)
getBlockTimeStrikesPage mpage mfilter = profile "getBlockTimeStrikesPage" $ do
  latestUnconfirmedBlockHeightV <- asks (BlockTime.latestUnconfirmedBlockHeight . blockTimeState)
  confirmedTipV <- asks (BlockTime.latestConfirmedBlock . blockTimeState)
  mlatestUnconfirmedBlockHeight <- liftIO $ TVar.readTVarIO latestUnconfirmedBlockHeightV
  mconfirmedTip <- liftIO $ TVar.readTVarIO confirmedTipV
  configBlockTimeStrikeGuessMinimumBlockAheadCurrentTip <- asks (configBlockTimeStrikeGuessMinimumBlockAheadCurrentTip . config)
  case (mlatestUnconfirmedBlockHeight, mconfirmedTip) of
    (Just latestUnconfirmedBlockHeight, Just confirmedTip) -> do
      let finalFilter =
            case maybe Nothing (blockTimeStrikeFilterClass . fst . unFilterRequest) mfilter of
              Nothing -> filter
              Just BlockTimeStrikeFilterClassGuessable ->
                let
                    minimumGuessableBlock = latestUnconfirmedBlockHeight + naturalFromPositive configBlockTimeStrikeGuessMinimumBlockAheadCurrentTip
                in
                  ( (BlockTimeStrikeBlock >=. minimumGuessableBlock) -- block height should match threshold
                  :filter
                  )
              Just BlockTimeStrikeFilterClassOutcomeKnown ->
                let
                  preFilter = ( [ BlockTimeStrikeBlock <=. latestUnconfirmedBlockHeight ]
                             ||. [ BlockTimeStrikeStrikeMediantime <=. fromIntegral (blockHeaderMediantime confirmedTip)]
                             ) -- block height should match threshold
                in
                ( preFilter
                ++ filter -- class will be handled inside lookup routine
                )
              Just BlockTimeStrikeFilterClassOutcomeUnknown ->
                ( (BlockTimeStrikeBlock >. latestUnconfirmedBlockHeight) -- block height should match threshold
                : filter -- class will be handled inside lookup routine
                )
      mret <- getBlockTimeStrikePast finalFilter latestUnconfirmedBlockHeight
      case mret of
        Nothing -> do
          throwJSON err500 ("something went wrong"::Text)
        Just ret -> return ret
    _ -> do
      let msg = "getBlockTimeStrikesPage: no confirmed block found yet"
      runLogging $  $(logError) msg
      throwJSON err500 msg
  where
    sort = maybe Descend (sortOrder . unFilterRequest) mfilter
    filter = (maybe [] (buildFilter . unFilterRequest) mfilter)
    getBlockTimeStrikePast finalFilter latestUnconfirmedBlockHeight = do
      recordsPerReply <- asks (configRecordsPerReply . config)
      let
          linesPerPage = maybe recordsPerReply (maybe recordsPerReply id . blockTimeStrikeFilterLinesPerPage . fst . unFilterRequest ) mfilter
      let
          eGuessesCount = case (maybe StrikeSortOrderDescend (maybe StrikeSortOrderDescend id . blockTimeStrikeFilterSort . fst . unFilterRequest) mfilter) of
            StrikeSortOrderAscend -> Left ()
            StrikeSortOrderDescend -> Left ()
            StrikeSortOrderAscendGuessesCount ->  Right ()
            StrikeSortOrderDescendGuessesCount -> Right ()
      case eGuessesCount of
        Left () -> pagingResult mpage linesPerPage finalFilter sort BlockTimeStrikeId -- select strikes with given filter first
          $ ( C.awaitForever $ \v@(Entity strikeId strike) -> do -- class
              case maybe Nothing (blockTimeStrikeFilterClass . fst . unFilterRequest) mfilter of
                Nothing -> do
                  mObserved <- if blockTimeStrikeBlock strike < latestUnconfirmedBlockHeight
                    then
                      -- now get possible observed data for strike
                      lift $ selectFirst
                        [ (BlockTimeStrikeObservedStrike ==. strikeId)
                        ]
                        []
                    else return Nothing
                  C.yield (v, mObserved) -- don't care about existence of observed data
                Just BlockTimeStrikeFilterClassGuessable-> do
                  C.yield (v, Nothing) -- strike have not been observed and finalFilter should ensure, that it is in the future with proper guess threshold
                Just BlockTimeStrikeFilterClassOutcomeUnknown-> do
                  C.yield (v, Nothing) -- haven't been observed
                Just BlockTimeStrikeFilterClassOutcomeKnown-> do
                  -- now get possible observed data for strike with possible custom filter
                  mObserved <- lift $ selectFirst
                    ( (BlockTimeStrikeObservedStrike ==. strikeId)
                    : (maybe [] ( buildFilter . unFilterRequest . mapFilter) mfilter)
                    )
                    []
                  case mObserved of
                    Just _ -> C.yield (v, mObserved) -- had been observed
                    _ -> return ()
            )
          .| ( C.awaitForever $ \(Entity strikeId strike, mObserved) -> do
              mguessesCount <- lift $ selectFirst
                [ CalculatedBlockTimeStrikeGuessesCountStrike ==. strikeId]
                []
              guessesCount <- case mguessesCount of
                Just (Entity _ guessesCount) -> -- results are already calculated
                  return (calculatedBlockTimeStrikeGuessesCountGuessesCount guessesCount)
                Nothing -> do -- fallback mode, recount online, which maybe a bad thing to do here TODO decide if it should be removed
                  guessesCount <- lift $ verifyNatural <$> count [ BlockTimeStrikeGuessStrike ==. strikeId ]
                  _ <- lift $ insert $! CalculatedBlockTimeStrikeGuessesCount
                    { calculatedBlockTimeStrikeGuessesCountStrike = strikeId
                    , calculatedBlockTimeStrikeGuessesCountGuessesCount = guessesCount
                    }
                  return guessesCount
              C.yield (BlockTimeStrikeWithGuessesCountPublic
                       { blockTimeStrikeWithGuessesCountPublicStrike = BlockTimeStrikePublic
                         { blockTimeStrikePublicObservedResult = maybe
                           Nothing
                           (\(Entity _ v)-> Just $! blockTimeStrikeObservedIsFast v)
                           mObserved
                         , blockTimeStrikePublicObservedBlockMediantime = maybe
                           Nothing
                           (\(Entity _ v)-> Just $! blockTimeStrikeObservedJudgementBlockMediantime v)
                           mObserved
                         , blockTimeStrikePublicObservedBlockHash = maybe
                           Nothing
                           (\(Entity _ v)-> Just $! blockTimeStrikeObservedJudgementBlockHash v)
                           mObserved
                         , blockTimeStrikePublicObservedBlockHeight = maybe
                           Nothing
                           (\(Entity _ v)-> Just $! blockTimeStrikeObservedJudgementBlockHeight v)
                           mObserved
                         , blockTimeStrikePublicBlock = blockTimeStrikeBlock strike
                         , blockTimeStrikePublicStrikeMediantime = blockTimeStrikeStrikeMediantime strike
                         , blockTimeStrikePublicCreationTime = blockTimeStrikeCreationTime strike
                         }
                       , blockTimeStrikeWithGuessesCountPublicGuessesCount = fromIntegral guessesCount
                       }
                      )
            )
        Right () -> pagingResult mpage linesPerPage [] sort CalculatedBlockTimeStrikeGuessesCountGuessesCount
          $ ( C.awaitForever $ \(Entity _ guessesCount) -> do
              mstrike <- lift $ selectFirst
                ((BlockTimeStrikeId ==. calculatedBlockTimeStrikeGuessesCountStrike guessesCount)
                 :finalFilter
                )
                []
              mObserved <- lift $ selectFirst
                ( (BlockTimeStrikeObservedStrike ==. calculatedBlockTimeStrikeGuessesCountStrike guessesCount)
                : (maybe [] ( buildFilter . unFilterRequest . mapFilter) mfilter)
                )[]
              case mstrike of
                Just (Entity _ strike) -> C.yield (BlockTimeStrikeWithGuessesCountPublic
                       { blockTimeStrikeWithGuessesCountPublicStrike = BlockTimeStrikePublic
                         { blockTimeStrikePublicObservedResult = maybe
                           Nothing
                           (\(Entity _ v)-> Just $! blockTimeStrikeObservedIsFast v)
                           mObserved
                         , blockTimeStrikePublicObservedBlockMediantime = maybe
                           Nothing
                           (\(Entity _ v)-> Just $! blockTimeStrikeObservedJudgementBlockMediantime v)
                           mObserved
                         , blockTimeStrikePublicObservedBlockHash = maybe
                           Nothing
                           (\(Entity _ v)-> Just $! blockTimeStrikeObservedJudgementBlockHash v)
                           mObserved
                         , blockTimeStrikePublicObservedBlockHeight = maybe
                           Nothing
                           (\(Entity _ v)-> Just $! blockTimeStrikeObservedJudgementBlockHeight v)
                           mObserved
                         , blockTimeStrikePublicBlock = blockTimeStrikeBlock strike
                         , blockTimeStrikePublicStrikeMediantime = blockTimeStrikeStrikeMediantime strike
                         , blockTimeStrikePublicCreationTime = blockTimeStrikeCreationTime strike
                         }
                       , blockTimeStrikeWithGuessesCountPublicGuessesCount =
                         fromIntegral (calculatedBlockTimeStrikeGuessesCountGuessesCount guessesCount)
                       }
                      )
                Nothing -> return ()
            )

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
      withDBNOTransactionROUnsafe "" $ do
        C.runConduit
          $ streamEntities
            [ BlockTimeStrikeBlock ==. blockHeight, BlockTimeStrikeStrikeMediantime ==. fromIntegral strikeMediantime ]
            BlockTimeStrikeId
            (PageSize ((fromPositive recordsPerReply) + 1))
            Descend
            (Range Nothing Nothing)
          .| ( let loop acc = do
                     mv <- C.await
                     case mv of
                       Just (Entity strikeId strike) -> do
                         mguessesCount <- lift $ selectFirst
                           [ CalculatedBlockTimeStrikeGuessesCountStrike ==. strikeId]
                           []
                         guessesCount <- case mguessesCount of
                           Just (Entity _ guessesCount) -> -- results are already calculated
                             return (calculatedBlockTimeStrikeGuessesCountGuessesCount guessesCount)
                           Nothing -> do -- fallback mode, recount online, which maybe a bad thing to do here TODO decide if it should be removed
                             guessesCount <- lift $ verifyNatural <$> count [ BlockTimeStrikeGuessStrike ==. strikeId ]
                             _ <- lift $ insert $! CalculatedBlockTimeStrikeGuessesCount
                               { calculatedBlockTimeStrikeGuessesCountStrike = strikeId
                               , calculatedBlockTimeStrikeGuessesCountGuessesCount = guessesCount
                               }
                             return guessesCount
                         mObserved <- lift $ selectFirst
                           [ BlockTimeStrikeObservedStrike ==. strikeId ]
                           []
                         return ( Just ( BlockTimeStrikeWithGuessesCountPublic
                                         { blockTimeStrikeWithGuessesCountPublicStrike = BlockTimeStrikePublic
                                           { blockTimeStrikePublicObservedResult = maybe
                                             Nothing
                                             (\(Entity _ v)-> Just $! blockTimeStrikeObservedIsFast v)
                                             mObserved
                                           , blockTimeStrikePublicObservedBlockMediantime = maybe
                                             Nothing
                                             (\(Entity _ v)-> Just $! blockTimeStrikeObservedJudgementBlockMediantime v)
                                             mObserved
                                           , blockTimeStrikePublicObservedBlockHash = maybe
                                             Nothing
                                             (\(Entity _ v)-> Just $! blockTimeStrikeObservedJudgementBlockHash v)
                                             mObserved
                                           , blockTimeStrikePublicObservedBlockHeight = maybe
                                             Nothing
                                             (\(Entity _ v)-> Just $! blockTimeStrikeObservedJudgementBlockHeight v)
                                             mObserved
                                           , blockTimeStrikePublicBlock = blockTimeStrikeBlock strike
                                           , blockTimeStrikePublicStrikeMediantime = blockTimeStrikeStrikeMediantime strike
                                           , blockTimeStrikePublicCreationTime = blockTimeStrikeCreationTime strike
                                           }
                                         , blockTimeStrikeWithGuessesCountPublicGuessesCount =
                                           fromIntegral guessesCount
                                         }
                                       )
                                )
                       Nothing -> return acc
               in loop Nothing
             )

