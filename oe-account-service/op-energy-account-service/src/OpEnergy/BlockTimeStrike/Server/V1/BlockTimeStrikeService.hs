{-- | This module implements BlockTime strike service.
 -}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE GADTs                     #-}
module OpEnergy.BlockTimeStrike.Server.V1.BlockTimeStrikeService
  ( createBlockTimeStrikeFuture
  , newTipHandlerLoop
  , getBlockTimeStrikesPage
  , getBlockTimeStrike
  ) where

import           Servant (err400, err500)
import           Control.Monad.Trans.Reader (ask, asks)
import           Control.Monad.Logger(logDebug, logError, logInfo)
import           Control.Monad(forever, void, when)
import           Data.Time.Clock(getCurrentTime)
import           Data.Time.Clock.POSIX(utcTimeToPOSIXSeconds, getPOSIXTime)
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
import           Data.OpEnergy.Client as Blockspan
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
import           OpEnergy.Account.Server.V1.Class (AppT, AppM, State(..), runLogging, profile, withDBTransaction, runLoggingIO, withDBNOTransactionROUnsafe )
import qualified OpEnergy.BlockTimeStrike.Server.V1.Class as BlockTime
import           OpEnergy.Account.Server.V1.AccountService (mgetPersonByAccountToken)
import qualified OpEnergy.BlockTimeStrike.Server.V1.BlockTimeScheduledStrikeCreation as BlockTimeScheduledStrikeCreation
import           OpEnergy.PagingResult

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
    _ <- observeStrikes confirmedTip
    BlockTimeScheduledStrikeCreation.maybeCreateStrikes confirmedTip
  where
    observeStrikes confirmedBlock = profile "moveFutureStrikesToPastStrikes" $ do
      configBlockspanURL <- asks (configBlockspanURL . config)
      recordsPerReply <- asks (configRecordsPerReply . config)
      state <- ask
      _ <- withDBTransaction "byBlock" $ do
        C.runConduit
          $ streamEntities
            [ BlockTimeStrikeBlock <=. blockHeaderHeight confirmedBlock
            ]
            BlockTimeStrikeId
            (PageSize (fromPositive recordsPerReply))
            Descend
            (Range Nothing Nothing)
          .| ( C.awaitForever $ \strikeE@(Entity strikeId _) -> do -- check if there
                                   -- all the data had been discovered already
               mResult <- lift $ selectFirst
                 [ BlockTimeStrikeResultStrike ==. strikeId
                 ][]
               mObserved <- lift $ selectFirst
                 [ BlockTimeStrikeObservedStrike ==. strikeId
                 ][]
               case (mObserved, mResult) of
                 (Just _, Just _ )-> return () -- all data already known
                 (mObserved, mResult) -> C.yield (strikeE, mObserved, mResult)
             )
          .| ( do
              let
                  loop (cnt::Int) = do
                    mv <- C.await
                    case mv of
                      Nothing -> do
                        liftIO $ runLoggingIO state $ $(logDebug) ("calculated results for " <> tshow cnt <> " strikes by observing block")
                      Just (Entity strikeId strike, mObserved, mResult) -> do
                        blockHeader <-
                          if blockHeaderHeight confirmedBlock == blockTimeStrikeBlock strike
                          then return confirmedBlock -- if we already have this block header
                          else liftIO $! Blockspan.withClient configBlockspanURL $ getBlockByHeight (blockTimeStrikeBlock strike)
                        -- there are 2 options here : either observed exist or not
                        when (mObserved == Nothing ) $ do  -- insert
                            now <- liftIO getPOSIXTime
                            void $ lift $ insert $ BlockTimeStrikeObserved
                              { blockTimeStrikeObservedStrike = strikeId
                              , blockTimeStrikeObservedBlockHash = blockHeaderHash blockHeader
                              , blockTimeStrikeObservedBlockMediantime = fromIntegral (blockHeaderMediantime blockHeader)
                              , blockTimeStrikeObservedCreationTime = now
                              }
                        -- calculate result
                        when (mResult == Nothing) $ do -- insert
                            now <- liftIO getPOSIXTime
                            void $ lift $ insert $ BlockTimeStrikeResult
                              { blockTimeStrikeResultResult =
                                  ( if fromIntegral (blockHeaderMediantime blockHeader) <= blockTimeStrikeStrikeMediantime strike
                                    then Fast
                                    else Slow
                                  )
                              , blockTimeStrikeResultCreationTime = now
                              , blockTimeStrikeResultStrike = strikeId
                              }
                        loop $! (cnt + 1)
              loop 0
            )
      latestUnconfirmedBlockHeightV <- asks (BlockTime.latestUnconfirmedBlockHeight . blockTimeState)
      mlatestUnconfirmedBlockHeight <- liftIO $ TVar.readTVarIO latestUnconfirmedBlockHeightV
      _ <- case mlatestUnconfirmedBlockHeight of
        Nothing -> return ()
        Just latestUnconfirmedBlockHeight -> do
          _ <- withDBTransaction "byTime" $ do
            C.runConduit
              $ streamEntities
                [ BlockTimeStrikeStrikeMediantime <=. (fromIntegral $ blockHeaderMediantime confirmedBlock)
                , BlockTimeStrikeBlock >. latestUnconfirmedBlockHeight -- don't resolve blocks, that had already discovered, but haven't been confirmed yet. It which will be observed soon and the result will be calculated by it's mediantime
                ]
                BlockTimeStrikeId
                (PageSize (fromPositive recordsPerReply))
                Descend
                (Range Nothing Nothing)
              .| ( C.awaitForever $ \v@(Entity strikeId _) -> do
                   isExist <- lift $ exists [ BlockTimeStrikeObservedStrike ==. strikeId]
                   if isExist
                     then return ()
                     else C.yield v
                 )
              .| ( do
                  let
                      loop (cnt::Int) = do
                        mv <- C.await
                        case mv of
                          Nothing -> do
                            liftIO $ runLoggingIO state $ $(logDebug) ("calculated results for " <> tshow cnt <> " strikes by reaching time")
                          Just (Entity strikeId _) -> do
                            now <- liftIO getPOSIXTime
                            void $ lift $ insert $! BlockTimeStrikeResult
                              { blockTimeStrikeResultResult = Slow
                              , blockTimeStrikeResultStrike = strikeId
                              , blockTimeStrikeResultCreationTime = now
                              }
                            loop $! (cnt + 1)
                  loop 0
                )
          return ()
      return ()

-- | returns list of BlockTimeStrikePublic records
getBlockTimeStrikesPage
  :: Maybe (Natural Int)
  -> Maybe (FilterRequest BlockTimeStrike BlockTimeStrikeFilter)
  -> AppM (PagingResult BlockTimeStrikePublic)
getBlockTimeStrikesPage mpage mfilter = profile "getBlockTimeStrikesPage" $ do
  latestUnconfirmedBlockHeightV <- asks (BlockTime.latestUnconfirmedBlockHeight . blockTimeState)
  mlatestUnconfirmedBlockHeight <- liftIO $ TVar.readTVarIO latestUnconfirmedBlockHeightV
  configBlockTimeStrikeGuessMinimumBlockAheadCurrentTip <- asks (configBlockTimeStrikeGuessMinimumBlockAheadCurrentTip . config)
  case mlatestUnconfirmedBlockHeight of
    Just latestUnconfirmedBlockHeight -> do
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
              Just BlockTimeStrikeFilterClassOutcomeKnown -> filter -- class will be handled inside lookup routine
              Just BlockTimeStrikeFilterClassOutcomeUnknown -> filter -- class will be handled inside lookup routine
      mret <- getBlockTimeStrikePast finalFilter
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
    getBlockTimeStrikePast finalFilter = do
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
          $ ( C.awaitForever $ \v@(Entity strikeId _) -> do -- class
              -- now get possible observed data for strike
              mObserved <- lift $ selectFirst
                ( (BlockTimeStrikeObservedStrike ==. strikeId)
                : (maybe [] ( buildFilter . unFilterRequest . mapFilter) mfilter)
                )
                []
              -- now get possible calculated outcome data for strike
              mResult <- lift $ selectFirst
                ( (BlockTimeStrikeResultStrike ==. strikeId)
                : []
                )
                []
              case maybe Nothing (blockTimeStrikeFilterClass . fst . unFilterRequest) mfilter of
                Nothing -> C.yield (v, mObserved, mResult) -- don't care about existence of observed data
                Just BlockTimeStrikeFilterClassGuessable-> case mResult of
                  Nothing -> C.yield (v, mObserved, mResult) -- strike have not been observed and finalFilter should ensure, that it is in the future with proper guess threshold
                  _ -> return () -- otherwise, block haven't matched the criteria and should be ignored
                Just BlockTimeStrikeFilterClassOutcomeUnknown-> case mResult of
                  Nothing-> C.yield (v, mObserved, mResult) -- haven't been observed
                  _ -> return ()
                Just BlockTimeStrikeFilterClassOutcomeKnown-> case mResult of
                  Just _ -> C.yield (v, mObserved, mResult) -- had been observed
                  _ -> return ()
            )
          .| ( C.awaitForever $ \(Entity strikeId strike, mObserved, mResult) -> do
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
              C.yield (BlockTimeStrikePublic
                       { blockTimeStrikePublicStrike = strike
                       , blockTimeStrikePublicGuessesCount = fromIntegral guessesCount
                       , blockTimeStrikePublicObserved =
                         maybe Nothing (\(Entity _ observed, Entity _ result)-> Just $! BlockTimeStrikeObservedPublic
                                          { blockTimeStrikeObservedPublicBlockHash = Just (blockTimeStrikeObservedBlockHash observed)
                                          , blockTimeStrikeObservedPublicCreationTime = blockTimeStrikeObservedCreationTime observed
                                          , blockTimeStrikeObservedPublicResult = blockTimeStrikeResultResult result
                                          , blockTimeStrikeObservedPublicBlockMediantime = Just (blockTimeStrikeObservedBlockMediantime observed)
                                          }
                                       ) $ do
                                       observed <- mObserved
                                       result <- mResult
                                       return (observed, result)
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
              mResult <- lift $ selectFirst
                ( (BlockTimeStrikeResultStrike ==. calculatedBlockTimeStrikeGuessesCountStrike guessesCount)
                : []
                )[]
              case mstrike of
                Just (Entity _ strike) -> C.yield (BlockTimeStrikePublic
                        { blockTimeStrikePublicStrike = strike
                        , blockTimeStrikePublicGuessesCount = fromIntegral (calculatedBlockTimeStrikeGuessesCountGuessesCount guessesCount)
                        , blockTimeStrikePublicObserved =
                         maybe Nothing (\(Entity _ observed, Entity _ result)-> Just $! BlockTimeStrikeObservedPublic
                                          { blockTimeStrikeObservedPublicBlockHash = Just (blockTimeStrikeObservedBlockHash observed)
                                          , blockTimeStrikeObservedPublicCreationTime = blockTimeStrikeObservedCreationTime observed
                                          , blockTimeStrikeObservedPublicResult = blockTimeStrikeResultResult result
                                          , blockTimeStrikeObservedPublicBlockMediantime = Just (blockTimeStrikeObservedBlockMediantime observed)
                                          }
                                       ) $ do
                                         observed <- mObserved
                                         result <- mResult
                                         return (observed, result)
                       }
                       )
                Nothing -> return ()
            )

-- | returns BlockTimeStrikePublic records
getBlockTimeStrike
  :: BlockHeight
  -> Natural Int
  -> AppM BlockTimeStrikePublic
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
                         resultsCnt <- lift $ count [ BlockTimeStrikeGuessStrike ==. strikeId ]
                         mObserved <- lift $ selectFirst
                           [ BlockTimeStrikeObservedStrike ==. strikeId ]
                           []
                         mResult <- lift $ selectFirst
                           [ BlockTimeStrikeResultStrike ==. strikeId ]
                           []
                         return ( Just ( BlockTimeStrikePublic
                                        { blockTimeStrikePublicStrike = strike
                                        , blockTimeStrikePublicGuessesCount = fromIntegral resultsCnt
                                        , blockTimeStrikePublicObserved = maybe
                                            Nothing
                                            (\(Entity _ observed, Entity _ result)-> Just $! BlockTimeStrikeObservedPublic
                                              { blockTimeStrikeObservedPublicBlockHash = Just (blockTimeStrikeObservedBlockHash observed)
                                              , blockTimeStrikeObservedPublicCreationTime = blockTimeStrikeObservedCreationTime observed
                                              , blockTimeStrikeObservedPublicResult = blockTimeStrikeResultResult result
                                              , blockTimeStrikeObservedPublicBlockMediantime = Just (blockTimeStrikeObservedBlockMediantime observed)
                                              }
                                            ) $ do
                                          observed <- mObserved
                                          result <- mResult
                                          return (observed, result)
                                        }
                                       )
                                )
                       Nothing -> return acc
               in loop Nothing
             )

