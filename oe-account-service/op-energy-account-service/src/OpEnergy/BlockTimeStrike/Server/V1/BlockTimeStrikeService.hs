{-- | This module implements BlockTime strike service.
 -}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards          #-}
module OpEnergy.BlockTimeStrike.Server.V1.BlockTimeStrikeService
  ( getBlockTimeStrikeFuture
  , createBlockTimeStrikeFuture
  , getBlockTimeStrikePast
  , newTipHandlerLoop
  , archiveFutureStrikesLoop
  , getBlockTimeStrikeExt
  ) where

import           Servant (err404, throwError, errBody)
import           Control.Monad.Trans.Reader (ask)
import qualified Data.List as List
import           Control.Monad.Logger(logDebug, logError, logInfo)
import           Control.Monad(forever, forM_)
import           Data.Time.Clock(getCurrentTime)
import           Data.Time.Clock.POSIX(utcTimeToPOSIXSeconds)
import qualified Control.Concurrent.STM.TVar as TVar
import qualified Control.Concurrent.MVar as MVar
import           Control.Concurrent(threadDelay)
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Conduit as C
import           Data.Conduit ((.|), runConduit)
import qualified Data.Conduit.List as C
import           Control.Monad.Trans
import           Database.Persist.Pagination
import           Control.Exception.Safe as E

import           Database.Persist.Postgresql
import           Prometheus(MonadMonitor)
import qualified Prometheus as P


import           Data.Text.Show (tshow)
import           Data.OpEnergy.Account.API.V1.Account
import           Data.OpEnergy.Client as Blockspan
import           Data.OpEnergy.API.V1.Block
import           Data.OpEnergy.API.V1.Natural
import           Data.OpEnergy.API.V1.Positive(fromPositive, naturalFromPositive)
import           Data.OpEnergy.Account.API.V1.BlockTimeStrike
import           Data.OpEnergy.Account.API.V1.BlockTimeStrikeGuess
import           Data.OpEnergy.Account.API.V1.BlockTimeStrikePublic
import           Data.OpEnergy.Account.API.V1.PagingResult
import           OpEnergy.Account.Server.V1.Config (Config(..))
import           OpEnergy.Account.Server.V1.Class (AppT, AppM, State(..), runLogging)
import qualified OpEnergy.BlockTimeStrike.Server.V1.Class as BlockTime
import qualified OpEnergy.Account.Server.V1.Metrics as Metrics(MetricsState(..))
import           OpEnergy.Account.Server.V1.AccountService (mgetPersonByAccountToken)
import qualified OpEnergy.BlockTimeStrike.Server.V1.BlockTimeScheduledFutureStrikeCreation as BlockTimeScheduledFutureStrikeCreation

-- | O(ln users) + O(strike future)
-- returns list BlockTimeStrikeFuture records. requires authenticated user
getBlockTimeStrikeFuture :: AccountToken-> AppM [BlockTimeStrikeFuture ]
getBlockTimeStrikeFuture token = do
  mperson <- mgetPersonByAccountToken token
  case mperson of
    Nothing -> do
      let err = "ERROR: getBlockTimeStrikeFuture: authentication failure"
      runLogging $ $(logError) err
      throwError err404 {errBody = BS.fromStrict (Text.encodeUtf8 err)}
    Just person -> getBlockTimeStrikeFuture person
  where
    getBlockTimeStrikeFuture :: (MonadIO m, MonadMonitor m) => Entity Person -> AppT m [BlockTimeStrikeFuture]
    getBlockTimeStrikeFuture (Entity _ _) = do
      State{ accountDBPool = pool
           , metrics = Metrics.MetricsState { Metrics.getBlockTimeStrikeFuture = getBlockTimeStrikeFuture
                                            }
           } <- ask
      P.observeDuration getBlockTimeStrikeFuture $ do
        liftIO $ flip runSqlPersistMPool pool $ selectList [] [] >>= return . List.map (\(Entity _ blocktimeStrikeFuture) -> blocktimeStrikeFuture)

-- | O(ln accounts).
-- Tries to create future block time strike. Requires authenticated user and blockheight should be in the future
createBlockTimeStrikeFuture :: AccountToken-> BlockHeight-> Natural Int-> AppM ()
createBlockTimeStrikeFuture token blockHeight nlocktime = do
  State{ config = Config{ configBlockTimeStrikeMinimumBlockAheadCurrentTip = configBlockTimeStrikeMinimumBlockAheadCurrentTip}
       , blockTimeState = BlockTime.State{ latestConfirmedBlock = latestConfirmedBlockV }
       } <- ask
  mlatestConfirmedBlock <- liftIO $ TVar.readTVarIO latestConfirmedBlockV
  case mlatestConfirmedBlock of
    Nothing -> do
      let err = "ERROR: createBlockTimeStrikeFuture: there is no current tip yet"
      runLogging $ $(logError) err
      throwError err404 {errBody = BS.fromStrict (Text.encodeUtf8 err)}
    Just tip
      | blockHeaderMediantime tip > fromIntegral nlocktime -> do
        let err = "ERROR: nlocktime is in the past, which is not expected"
        runLogging $ $(logError) err
        throwError err404 {errBody = BS.fromStrict (Text.encodeUtf8 err)}
    Just tip
      | blockHeaderHeight tip + naturalFromPositive configBlockTimeStrikeMinimumBlockAheadCurrentTip > blockHeight -> do
        let msg = "ERROR: createBlockTimeStrikeFuture: block height for new block time strike should be in the future + minimum configBlockTimeStrikeMinimumBlockAheadCurrentTip"
        runLogging $ $(logError) msg
        throwError err404
    _ -> do
      mperson <- mgetPersonByAccountToken token
      case mperson of
        Nothing -> do
          let err = "ERROR: createBlockTimeStrikeFuture: person was not able to authenticate itself"
          runLogging $ $(logError) err
          throwError err404 {errBody = BS.fromStrict (Text.encodeUtf8 err)}
        Just person -> createBlockTimeStrikeFutureEsnuredConditions person
  where
    createBlockTimeStrikeFutureEsnuredConditions :: (MonadIO m, MonadMonitor m) => (Entity Person) -> AppT m ()
    createBlockTimeStrikeFutureEsnuredConditions _ = do
      State{ accountDBPool = pool
           , metrics = Metrics.MetricsState { Metrics.createBlockTimeStrikeFuture = createBlockTimeStrikeFuture
                                            }
           } <- ask
      P.observeDuration createBlockTimeStrikeFuture $ do
        nowUTC <- liftIO getCurrentTime
        let now = utcTimeToPOSIXSeconds nowUTC
        liftIO $ flip runSqlPersistMPool pool $ insert_ $! BlockTimeStrikeFuture
          { blockTimeStrikeFutureBlock = blockHeight
          , blockTimeStrikeFutureNlocktime = fromIntegral nlocktime
          , blockTimeStrikeFutureCreationTime = now
          }

-- | O(ln accounts) + O(BlockTimeStrikePast).
-- returns list BlockTimeStrikePast records. requires authenticated user
getBlockTimeStrikePast :: AccountToken-> AppM [ BlockTimeStrikePast]
getBlockTimeStrikePast token = do
  mperson <- mgetPersonByAccountToken token
  case mperson of
    Nothing -> do
      let err = "ERROR: getBlockTimeStrikePast: person was not able to authenticate itself"
      runLogging $ $(logError) err
      throwError err404 {errBody = BS.fromStrict (Text.encodeUtf8 err)}
    Just person -> getBlockTimeStrikePast person
  where
    getBlockTimeStrikePast :: (MonadIO m, MonadMonitor m) => (Entity Person) -> AppT m [ BlockTimeStrikePast]
    getBlockTimeStrikePast _ = do
      State{ accountDBPool = pool
           , metrics = Metrics.MetricsState { Metrics.getBlockTimeStrikePast = getBlockTimeStrikePast
                                            }
           } <- ask
      P.observeDuration getBlockTimeStrikePast $ do
        liftIO $ flip runSqlPersistMPool pool $ selectList [] [] >>= return . List.map (\(Entity _ blocktimeStrikeFuture) -> blocktimeStrikeFuture)

-- | O(ln accounts) + O(BlockTimeStrikePast).
-- returns list of BlockTimeStrikePastPublic records. Do not require authentication.
getBlockTimeStrikeExt :: Maybe (Natural Int)-> AppM (PagingResult BlockTimeStrikePastPublic)
getBlockTimeStrikeExt mpage = do
  eret <- getBlockTimeStrikePast
  case eret of
    Left some -> do
      let err = "ERROR: getBlockTimeStrikeExt: " <> Text.pack (show some)
      runLogging $ $(logError) err
      throwError err404 {errBody = BS.fromStrict (Text.encodeUtf8 err)}
    Right ret -> return ret
  where
    page = case mpage of
      Nothing -> 0
      Just ret -> ret
    getBlockTimeStrikePast = do
      State{ accountDBPool = pool
           , config = Config{ configRecordsPerReply = recordsPerReply}
           , metrics = Metrics.MetricsState { Metrics.getBlockTimeStrikePast = getBlockTimeStrikePast
                                            }
           } <- ask
      P.observeDuration getBlockTimeStrikePast $ do
        eret <- E.handle
          (\(err::SomeException) -> return (Left err))
          $ liftIO $ flip runSqlPersistMPool pool $ do
            totalCount <- count ([]::[Filter BlockTimeStrikePast])
            if (fromNatural page) * (fromPositive recordsPerReply) >= totalCount
              then return (Right (totalCount, [])) -- page out of range
              else do
                pageResults <- runConduit
                  $ streamEntities
                    []
                    BlockTimeStrikePastObservedBlockMediantime
                    (PageSize ((fromPositive recordsPerReply) + 1))
                    Descend
                    (Range Nothing Nothing)
                  .| (C.drop (fromNatural page * fromPositive recordsPerReply) >> C.awaitForever C.yield) -- navigate to page
                  .| ( C.awaitForever $ \(Entity strikeId strike) -> do
                      resultsCnt <- lift $ count [ BlockTimeStrikePastGuessStrike ==. strikeId ]
                      C.yield (BlockTimeStrikePastPublic {pastStrike = strike, guessesCount = fromIntegral resultsCnt})
                    )
                  .| C.take (fromPositive recordsPerReply + 1) -- we take +1 to understand if there is a next page available
                return (Right (totalCount, pageResults))
        case eret of
          Left some -> return (Left some)
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


-- | this function is an entry point for a process, that creates blocktime past strikes when such block is being
-- confirmed. After BlockTimeStrikePast creation, it will add record to the BlockTimeStrikeFutureObservedBlock table
-- in order to notify hndlers in the chain (currently only guess game), which should move appropriate references
-- to such blocktime future strike into past strikes. Then, those subsequent handlers should create
-- BlockTimeStrikeFutureReadyToRemove record, which should be handled by archiveFutureStrikesLoop function
newTipHandlerLoop :: (MonadIO m, MonadMonitor m) => AppT m ()
newTipHandlerLoop = forever $ do
  State{ blockTimeState = BlockTime.State
         { blockTimeStrikeCurrentTip = currentTipV
         }
       , metrics = Metrics.MetricsState { Metrics.getBlockTimeStrikeFuture = getBlockTimeStrikeFuture
                                        }
       } <- ask
  -- get new current tip notification from upstream handler
  currentTip <- liftIO $ MVar.takeMVar currentTipV
  runLogging $ $(logInfo) $ "BlockTimeStrikeService: tipHandler: received new current tip height: " <> (tshow $ blockHeaderHeight currentTip)
  -- find out any future strikes <= new current tip
  P.observeDuration getBlockTimeStrikeFuture $ do
    moveFutureStrikesToPastStrikes currentTip
    BlockTimeScheduledFutureStrikeCreation.maybeCreateFutureStrikes currentTip
  where
    moveFutureStrikesToPastStrikes currentTip = do
      State{ config = Config { configBlockspanURL = configBlockspanURL }
           , accountDBPool = pool
           } <- ask
      liftIO $ flip runSqlPersistMPool pool $ do
        futureBlocks <- selectList [ BlockTimeStrikeFutureBlock <=. blockHeaderHeight currentTip] []
        forM_ futureBlocks $ \(Entity futureStrikeKey futureStrike) -> do
          nowUTC <- liftIO getCurrentTime
          let now = utcTimeToPOSIXSeconds nowUTC
          -- create past block
          blockHeader <-
            if blockHeaderHeight currentTip == blockTimeStrikeFutureBlock futureStrike
            then return currentTip -- if we already have this block header
            else liftIO $! Blockspan.withClient configBlockspanURL $ getBlockByHeight (blockTimeStrikeFutureBlock futureStrike)
          -- it is possible, that BlockTimeStrikePast already had been created, so we need to check if such strike exists and ignore it
          mexist <- selectFirst
            [ BlockTimeStrikePastBlock ==. blockTimeStrikeFutureBlock futureStrike
            , BlockTimeStrikePastNlocktime ==. blockTimeStrikeFutureNlocktime futureStrike
            ]
            []
          case mexist of
            Nothing -> do
              let pastStrike = BlockTimeStrikePast
                    { blockTimeStrikePastBlock = blockTimeStrikeFutureBlock futureStrike
                    , blockTimeStrikePastNlocktime = blockTimeStrikeFutureNlocktime futureStrike
                    , blockTimeStrikePastFutureStrikeCreationTime = blockTimeStrikeFutureCreationTime futureStrike
                    , blockTimeStrikePastObservedBlockMediantime = fromIntegral $ blockHeaderMediantime blockHeader
                    , blockTimeStrikePastObservedBlockHash = blockHeaderHash blockHeader
                    , blockTimeStrikePastCreationTime = now
                    , blockTimeStrikePastObservedResult =
                      if fromIntegral (blockHeaderMediantime blockHeader) <= blockTimeStrikeFutureNlocktime futureStrike
                      then Fast
                      else Slow
                    }
              pastStrikeKey <- insert pastStrike
              let observedBlock = BlockTimeStrikeFutureObservedBlock
                    { blockTimeStrikeFutureObservedBlockFutureStrike = futureStrikeKey
                    , blockTimeStrikeFutureObservedBlockPastStrike = pastStrikeKey
                    , blockTimeStrikeFutureObservedBlockFutureStrikeBlock = blockTimeStrikeFutureBlock futureStrike
                    , blockTimeStrikeFutureObservedBlockFutureStrikeNlocktime = blockTimeStrikeFutureNlocktime futureStrike
                    }
              -- if there were no past strike yet, then observed is not exist as well
              _ <- insert observedBlock -- we are sure, that no observed block exist yet
              return ()
            Just _ -> -- past strike exists, ignore it
              return ()

-- | this function is an entry point of thread, that removes BlockTimeStrikeFuture record when all the guesses had been
-- already moved into results and thus there are no more references to the current record
archiveFutureStrikesLoop :: (MonadIO m, MonadMonitor m) => AppT m ()
archiveFutureStrikesLoop = forever $ do
  State{ config = Config {configSchedulerPollRateSecs = configSchedulerPollRateSecs}} <- ask
  runLogging $ $(logDebug) $ "archiveFutureStrikesLoop: iteration"
  archiveFutureStrikesIteration
  liftIO $ threadDelay $ 1000000 * (fromIntegral configSchedulerPollRateSecs) -- sleep for poll rate secs
  where
    archiveFutureStrikesIteration :: (MonadIO m, MonadMonitor m) => AppT m ()
    archiveFutureStrikesIteration = do
      State{ accountDBPool = pool
           } <- ask
      liftIO $ flip runSqlPersistMPool pool $ do
        (futureStrikesToRemove :: [Entity BlockTimeStrikeFutureReadyToRemove]) <- selectList [] []
        forM_ futureStrikesToRemove $ \(Entity futureStrikeToRemoveKey futureStrikeToRemove) -> do
          delete futureStrikeToRemoveKey -- clean BlockTimeStrikeFutureReadyToRemove record as well
          delete (blockTimeStrikeFutureReadyToRemoveFutureStrike futureStrikeToRemove) -- clean blocktime strike future
