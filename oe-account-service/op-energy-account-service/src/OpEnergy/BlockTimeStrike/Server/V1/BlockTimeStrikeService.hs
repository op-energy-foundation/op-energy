{-- | This module implements BlockTime strike service.
 -}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards          #-}
module OpEnergy.BlockTimeStrike.Server.V1.BlockTimeStrikeService
  ( getBlockTimeStrikeFuture
  , getBlockTimeStrikeFuturePage
  , createBlockTimeStrikeFuture
  , getBlockTimeStrikePast
  , newTipHandlerLoop
  , archiveFutureStrikesLoop
  , getBlockTimeStrikePastPage
  , getBlockTimeStrikePastExt
  ) where

import           Servant (err400, err404, err500, throwError, errBody)
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
import qualified Data.Conduit.List as C
import           Control.Monad.Trans
import           Control.Exception.Safe as E

import           Database.Persist.Postgresql
import           Prometheus(MonadMonitor)


import           Data.Text.Show (tshow)
import           Data.OpEnergy.Account.API.V1.Account
import           Data.OpEnergy.Client as Blockspan
import           Data.OpEnergy.API.V1.Block
import           Data.OpEnergy.API.V1.Natural
import           Data.OpEnergy.API.V1.Positive( naturalFromPositive)
import           Data.OpEnergy.Account.API.V1.BlockTimeStrike
import           Data.OpEnergy.Account.API.V1.BlockTimeStrikeGuess
import           Data.OpEnergy.Account.API.V1.BlockTimeStrikePublic
import           Data.OpEnergy.Account.API.V1.PagingResult
import           Data.OpEnergy.Account.API.V1.FilterRequest
import           OpEnergy.Account.Server.V1.Config (Config(..))
import           OpEnergy.Account.Server.V1.Class (AppT, AppM, State(..), runLogging, profile, withDBTransaction)
import qualified OpEnergy.BlockTimeStrike.Server.V1.Class as BlockTime
import           OpEnergy.Account.Server.V1.AccountService (mgetPersonByAccountToken)
import qualified OpEnergy.BlockTimeStrike.Server.V1.BlockTimeScheduledFutureStrikeCreation as BlockTimeScheduledFutureStrikeCreation
import           OpEnergy.PagingResult

-- | O(ln users) + O(strike future)
-- returns list BlockTimeStrikeFuture records. requires authenticated user
getBlockTimeStrikeFuture :: AccountToken-> AppM [BlockTimeStrikeFuture ]
getBlockTimeStrikeFuture token = profile "getBlockTimeStrikeFuture" $ do
  mperson <- mgetPersonByAccountToken token
  case mperson of
    Nothing -> do
      let err = "ERROR: getBlockTimeStrikeFuture: authentication failure"
      runLogging $ $(logError) err
      throwError err400 {errBody = BS.fromStrict (Text.encodeUtf8 err)}
    Just person -> do
      mret <- getBlockTimeStrikeFuture person
      case mret of
        Just ret -> return ret
        Nothing -> throwError err500 {errBody = "something went wrong"}
  where
    getBlockTimeStrikeFuture :: (MonadIO m, MonadMonitor m) => Entity Person -> AppT m (Maybe [BlockTimeStrikeFuture])
    getBlockTimeStrikeFuture (Entity _ _) = do
      withDBTransaction "" $ selectList [] [] >>= return . List.map (\(Entity _ blocktimeStrikeFuture) -> blocktimeStrikeFuture)

-- | O(ln users) + O(strike future)
-- returns list BlockTimeStrikeFuture records
getBlockTimeStrikeFuturePage
  :: Maybe (Natural Int)
  -> Maybe (FilterRequest BlockTimeStrikeFuture BlockTimeStrikeFutureFilter)
  -> AppM (PagingResult BlockTimeStrikeFuture)
getBlockTimeStrikeFuturePage mpage mfilter = profile "getBlockTimeStrikeFuturePage" $ do
  mret <- getBlockTimeStrikeFuture
  runLogging $ $(logDebug) $ "filter: " <> (Text.pack $ show mfilter)
  case mret of
    Nothing -> do
      throwError err500 {errBody = "something went wrong"}
    Just ret -> return ret
  where
    filter = maybe [] (buildFilter . unFilterRequest) mfilter
    getBlockTimeStrikeFuture :: (MonadIO m, MonadMonitor m, MonadCatch m) => AppT m (Maybe (PagingResult BlockTimeStrikeFuture))
    getBlockTimeStrikeFuture = do
      pagingResult mpage filter BlockTimeStrikeFutureCreationTime $ C.map $ \(Entity _ v) -> v

-- | O(ln accounts).
-- Tries to create future block time strike. Requires authenticated user and blockheight should be in the future
createBlockTimeStrikeFuture :: AccountToken-> BlockHeight-> Natural Int-> AppM ()
createBlockTimeStrikeFuture token blockHeight nlocktime = profile "createBlockTimeStrikeFuture" $ do
  State{ config = Config{ configBlockTimeStrikeMinimumBlockAheadCurrentTip = configBlockTimeStrikeMinimumBlockAheadCurrentTip}
       , blockTimeState = BlockTime.State{ latestConfirmedBlock = latestConfirmedBlockV }
       } <- ask
  mlatestConfirmedBlock <- liftIO $ TVar.readTVarIO latestConfirmedBlockV
  case mlatestConfirmedBlock of
    Nothing -> do
      let err = "ERROR: createBlockTimeStrikeFuture: there is no current tip yet"
      runLogging $ $(logError) err
      throwError err400 {errBody = BS.fromStrict (Text.encodeUtf8 err)}
    Just tip
      | blockHeaderMediantime tip > fromIntegral nlocktime -> do
        let err = "ERROR: nlocktime is in the past, which is not expected"
        runLogging $ $(logError) err
        throwError err400 {errBody = BS.fromStrict (Text.encodeUtf8 err)}
    Just tip
      | blockHeaderHeight tip + naturalFromPositive configBlockTimeStrikeMinimumBlockAheadCurrentTip > blockHeight -> do
        let msg = "ERROR: createBlockTimeStrikeFuture: block height for new block time strike should be in the future + minimum configBlockTimeStrikeMinimumBlockAheadCurrentTip"
        runLogging $ $(logError) msg
        throwError err400 {errBody = BS.fromStrict (Text.encodeUtf8 msg)}
    _ -> do
      mperson <- mgetPersonByAccountToken token
      case mperson of
        Nothing -> do
          let err = "ERROR: createBlockTimeStrikeFuture: person was not able to authenticate itself"
          runLogging $ $(logError) err
          throwError err400 {errBody = BS.fromStrict (Text.encodeUtf8 err)}
        Just person -> do
          mret <- createBlockTimeStrikeFutureEsnuredConditions person
          case mret of
            Just ret -> return ret
            Nothing -> do
              throwError err500 {errBody = "something went wrong"}
  where
    createBlockTimeStrikeFutureEsnuredConditions :: (MonadIO m, MonadMonitor m) => (Entity Person) -> AppT m (Maybe ())
    createBlockTimeStrikeFutureEsnuredConditions _ = do
      nowUTC <- liftIO getCurrentTime
      let now = utcTimeToPOSIXSeconds nowUTC
      withDBTransaction "" $ insert_ $! BlockTimeStrikeFuture
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
    Just person -> do
      mret <- getBlockTimeStrikePast person
      case mret of
        Nothing -> throwError err500 { errBody = "something went wrong"}
        Just ret -> return ret
  where
    getBlockTimeStrikePast :: (MonadIO m, MonadMonitor m) => (Entity Person) -> AppT m (Maybe [ BlockTimeStrikePast])
    getBlockTimeStrikePast _ = do
      withDBTransaction "" $ selectList [] [] >>= return . List.map (\(Entity _ blocktimeStrikeFuture) -> blocktimeStrikeFuture)

-- | O(ln accounts) + O(BlockTimeStrikePast).
-- returns list of BlockTimeStrikePastPublic records. Do not require authentication.
getBlockTimeStrikePastExt
  :: Maybe (Natural Int)
  -> AppM (PagingResult BlockTimeStrikePastPublic)
getBlockTimeStrikePastExt mpage = getBlockTimeStrikePastPage mpage Nothing

-- | O(ln accounts) + O(BlockTimeStrikePast).
-- returns list of BlockTimeStrikePastPublic records. Do not require authentication.
getBlockTimeStrikePastPage
  :: Maybe (Natural Int)
  -> Maybe (FilterRequest BlockTimeStrikePast BlockTimeStrikePastFilter)
  -> AppM (PagingResult BlockTimeStrikePastPublic)
getBlockTimeStrikePastPage mpage mfilter = profile "getBlockTimeStrikePastPage" $ do
  mret <- getBlockTimeStrikePast
  case mret of
    Nothing -> do
      throwError err500 {errBody = "something went wrong"}
    Just ret -> return ret
  where
    getBlockTimeStrikePast = do
      pagingResult mpage (maybe [] (buildFilter . unFilterRequest) mfilter) BlockTimeStrikePastObservedBlockMediantime
        $ ( C.awaitForever $ \(Entity strikeId strike) -> do
            resultsCnt <- lift $ count [ BlockTimeStrikePastGuessStrike ==. strikeId ]
            C.yield (BlockTimeStrikePastPublic {blockTimeStrikePastPublicPastStrike = strike, blockTimeStrikePastPublicGuessesCount = fromIntegral resultsCnt})
          )

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
       } <- ask
  -- get new current tip notification from upstream handler
  currentTip <- liftIO $ MVar.takeMVar currentTipV
  runLogging $ $(logInfo) $ "BlockTimeStrikeService: tipHandler: received new current tip height: " <> (tshow $ blockHeaderHeight currentTip)
  -- find out any future strikes <= new current tip
  profile "newTipHandlerIteration" $ do
    _ <- moveFutureStrikesToPastStrikes currentTip
    BlockTimeScheduledFutureStrikeCreation.maybeCreateFutureStrikes currentTip
  where
    moveFutureStrikesToPastStrikes currentTip = profile "moveFutureStrikesToPastStrikes" $ do
      State{ config = Config { configBlockspanURL = configBlockspanURL }
           } <- ask
      withDBTransaction "" $ do
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
  _ <- archiveFutureStrikesIteration
  liftIO $ threadDelay $ 1000000 * (fromIntegral configSchedulerPollRateSecs) -- sleep for poll rate secs
  where
    archiveFutureStrikesIteration :: (MonadIO m, MonadMonitor m) => AppT m (Maybe ())
    archiveFutureStrikesIteration = profile "archiveFutureStrikesIteration" $ do
      withDBTransaction "" $ do
        (futureStrikesToRemove :: [Entity BlockTimeStrikeFutureReadyToRemove]) <- selectList [] []
        forM_ futureStrikesToRemove $ \(Entity futureStrikeToRemoveKey futureStrikeToRemove) -> do
          delete futureStrikeToRemoveKey -- clean BlockTimeStrikeFutureReadyToRemove record as well
          delete (blockTimeStrikeFutureReadyToRemoveFutureStrike futureStrikeToRemove) -- clean blocktime strike future
