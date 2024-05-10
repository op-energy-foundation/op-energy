{-- | This module implements BlockTime strike service.
 -}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards          #-}
module OpEnergy.BlockTimeStrike.Server.V1.BlockTimeStrikeService
  ( getBlockTimeStrikeFuturePage
  , createBlockTimeStrikeFuture
  , newTipHandlerLoop
  , getBlockTimeStrikePastPage
  , getBlockTimeStrikePage
  ) where

import           Servant (err400, err500, throwError, errBody)
import           Control.Monad.Trans.Reader (ask)
import           Control.Monad.Logger(logDebug, logError, logInfo)
import           Control.Monad(forever, forM_)
import           Data.Time.Clock(getCurrentTime)
import           Data.Time.Clock.POSIX(utcTimeToPOSIXSeconds)
import qualified Control.Concurrent.STM.TVar as TVar
import qualified Control.Concurrent.MVar as MVar
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Conduit as C
import qualified Data.Conduit.List as C
import           Control.Monad.Trans
import           Control.Exception.Safe as E

import           Database.Persist.Postgresql
import           Database.Persist.Pagination
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
import qualified OpEnergy.BlockTimeStrike.Server.V1.BlockTimeScheduledStrikeCreation as BlockTimeScheduledStrikeCreation
import           OpEnergy.PagingResult

-- | O(ln users) + O(strike future)
-- returns list BlockTimeStrikeFuture records
getBlockTimeStrikeFuturePage
  :: Maybe (Natural Int)
  -> Maybe (FilterRequest BlockTimeStrike BlockTimeStrikeFilter)
  -> AppM (PagingResult BlockTimeStrike)
getBlockTimeStrikeFuturePage mpage mfilter = profile "getBlockTimeStrikeFuturePage" $ do
  mret <- getBlockTimeStrikeFuture
  runLogging $ $(logDebug) $ "filter: " <> (Text.pack $ show mfilter)
  case mret of
    Nothing -> do
      throwError err500 {errBody = "something went wrong"}
    Just ret -> return ret
  where
    filter = (BlockTimeStrikeObservedBlockHash ==. Nothing):(maybe [] (buildFilter . unFilterRequest) mfilter)
    sort = maybe Descend (sortOrder . unFilterRequest) mfilter
    getBlockTimeStrikeFuture :: (MonadIO m, MonadMonitor m, MonadCatch m) => AppT m (Maybe (PagingResult BlockTimeStrike))
    getBlockTimeStrikeFuture = do
      pagingResult mpage filter sort BlockTimeStrikeCreationTime $ C.map $ \(Entity _ v) -> v

-- | O(ln accounts).
-- Tries to create future block time strike. Requires authenticated user and blockheight should be in the future
createBlockTimeStrikeFuture :: AccountToken-> BlockHeight-> Natural Int-> AppM ()
createBlockTimeStrikeFuture token blockHeight nlocktime = profile "createBlockTimeStrike" $ do
  State{ config = Config{ configBlockTimeStrikeMinimumBlockAheadCurrentTip = configBlockTimeStrikeMinimumBlockAheadCurrentTip}
       , blockTimeState = BlockTime.State{ latestConfirmedBlock = latestConfirmedBlockV }
       } <- ask
  mlatestConfirmedBlock <- liftIO $ TVar.readTVarIO latestConfirmedBlockV
  case mlatestConfirmedBlock of
    Nothing -> do
      let err = "ERROR: createBlockTimeStrike: there is no current tip yet"
      runLogging $ $(logError) err
      throwError err400 {errBody = BS.fromStrict (Text.encodeUtf8 err)}
    Just tip
      | blockHeaderMediantime tip > fromIntegral nlocktime -> do
        let err = "ERROR: nlocktime is in the past, which is not expected"
        runLogging $ $(logError) err
        throwError err400 {errBody = BS.fromStrict (Text.encodeUtf8 err)}
    Just tip
      | blockHeaderHeight tip + naturalFromPositive configBlockTimeStrikeMinimumBlockAheadCurrentTip > blockHeight -> do
        let msg = "ERROR: createBlockTimeStrike: block height for new block time strike should be in the future + minimum configBlockTimeStrikeMinimumBlockAheadCurrentTip"
        runLogging $ $(logError) msg
        throwError err400 {errBody = BS.fromStrict (Text.encodeUtf8 msg)}
    _ -> do
      mperson <- mgetPersonByAccountToken token
      case mperson of
        Nothing -> do
          let err = "ERROR: createBlockTimeStrike: person was not able to authenticate itself"
          runLogging $ $(logError) err
          throwError err400 {errBody = BS.fromStrict (Text.encodeUtf8 err)}
        Just person -> do
          mret <- createBlockTimeStrikeEnsuredConditions person
          case mret of
            Just ret -> return ret
            Nothing -> do
              throwError err500 {errBody = "something went wrong"}
  where
    createBlockTimeStrikeEnsuredConditions :: (MonadIO m, MonadMonitor m) => (Entity Person) -> AppT m (Maybe ())
    createBlockTimeStrikeEnsuredConditions _ = do
      nowUTC <- liftIO getCurrentTime
      let now = utcTimeToPOSIXSeconds nowUTC
      withDBTransaction "" $ insert_ $! BlockTimeStrike
        { blockTimeStrikeBlock = blockHeight
        , blockTimeStrikeNlocktime = fromIntegral nlocktime
        , blockTimeStrikeCreationTime = now
        , blockTimeStrikeObservedResult = Nothing
        , blockTimeStrikeObservedBlockMediantime = Nothing
        , blockTimeStrikeObservedBlockHash = Nothing
        }

-- | O(ln accounts) + O(BlockTimeStrikePast).
-- returns list of BlockTimeStrikePastPublic records. Do not require authentication.
getBlockTimeStrikePastPage
  :: Maybe (Natural Int)
  -> Maybe (FilterRequest BlockTimeStrike BlockTimeStrikeFilter)
  -> AppM (PagingResult BlockTimeStrikePublic)
getBlockTimeStrikePastPage mpage mfilter = profile "getBlockTimeStrikePastPage" $ do
  mret <- getBlockTimeStrikePast
  case mret of
    Nothing -> do
      throwError err500 {errBody = "something went wrong"}
    Just ret -> return ret
  where
    sort = maybe Descend (sortOrder . unFilterRequest) mfilter
    filter = (BlockTimeStrikeObservedBlockHash !=. Nothing):(maybe [] (buildFilter . unFilterRequest) mfilter)
    getBlockTimeStrikePast = do
      pagingResult mpage filter sort BlockTimeStrikeObservedBlockMediantime
        $ ( C.awaitForever $ \(Entity strikeId strike) -> do
            resultsCnt <- lift $ count [ BlockTimeStrikeGuessStrike ==. strikeId ]
            C.yield (BlockTimeStrikePublic {blockTimeStrikePublicStrike = strike, blockTimeStrikePublicGuessesCount = fromIntegral resultsCnt})
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
    _ <- observeStrikes currentTip
    BlockTimeScheduledStrikeCreation.maybeCreateStrikes currentTip
  where
    observeStrikes currentTip = profile "moveFutureStrikesToPastStrikes" $ do
      State{ config = Config { configBlockspanURL = configBlockspanURL }
           } <- ask
      withDBTransaction "" $ do
        futureBlocks <- selectList [ BlockTimeStrikeBlock <=. blockHeaderHeight currentTip
                                   , BlockTimeStrikeObservedBlockHash ==. Nothing
                                   ] []
        forM_ futureBlocks $ \(Entity futureStrikeKey futureStrike) -> do
          -- create past block
          blockHeader <-
            if blockHeaderHeight currentTip == blockTimeStrikeBlock futureStrike
            then return currentTip -- if we already have this block header
            else liftIO $! Blockspan.withClient configBlockspanURL $ getBlockByHeight (blockTimeStrikeBlock futureStrike)
          -- it is possible, that BlockTimeStrikePast already had been created, so we need to check if such strike exists and ignore it
          update futureStrikeKey
            [ BlockTimeStrikeObservedResult =.
              ( Just $!
                  if fromIntegral (blockHeaderMediantime blockHeader) <= blockTimeStrikeNlocktime futureStrike
                  then Fast
                  else Slow
              )
            , BlockTimeStrikeObservedBlockMediantime =. ( Just $! fromIntegral $ blockHeaderMediantime blockHeader)
            , BlockTimeStrikeObservedBlockHash =. (Just $! blockHeaderHash blockHeader)
            ]


-- | returns list of BlockTimeStrikePublic records
getBlockTimeStrikePage
  :: Maybe (Natural Int)
  -> Maybe (FilterRequest BlockTimeStrike BlockTimeStrikeFilter)
  -> AppM (PagingResult BlockTimeStrikePublic)
getBlockTimeStrikePage mpage mfilter = profile "getBlockTimeStrikePage" $ do
  mret <- getBlockTimeStrikePast
  case mret of
    Nothing -> do
      throwError err500 {errBody = "something went wrong"}
    Just ret -> return ret
  where
    sort = maybe Descend (sortOrder . unFilterRequest) mfilter
    filter = (maybe [] (buildFilter . unFilterRequest) mfilter)
    getBlockTimeStrikePast = do
      pagingResult mpage filter sort BlockTimeStrikeId
        $ ( C.awaitForever $ \(Entity strikeId strike) -> do
            resultsCnt <- lift $ count [ BlockTimeStrikeGuessStrike ==. strikeId ]
            C.yield (BlockTimeStrikePublic {blockTimeStrikePublicStrike = strike, blockTimeStrikePublicGuessesCount = fromIntegral resultsCnt})
          )
