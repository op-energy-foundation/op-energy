{-- | This module implements BlockTime strike service.
 -}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE GADTs                     #-}
module OpEnergy.BlockTimeStrike.Server.V1.BlockTimeStrikeService
  ( getBlockTimeStrikeFuturePage
  , createBlockTimeStrikeFuture
  , newTipHandlerLoop
  , getBlockTimeStrikePastPage
  , getBlockTimeStrikesPage
  , getBlockTimeStrike
  ) where

import           Servant (err400, err500)
import           Control.Monad.Trans.Reader (ask, asks)
import           Control.Monad.Logger(logDebug, logError, logInfo)
import           Control.Monad(forever, when)
import           Data.Time.Clock(getCurrentTime)
import           Data.Time.Clock.POSIX(utcTimeToPOSIXSeconds)
import qualified Control.Concurrent.STM.TVar as TVar
import qualified Control.Concurrent.MVar as MVar
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Conduit( (.|) )
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
      throwJSON err500 ("something went wrong"::Text)
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
createBlockTimeStrikeFuture token blockHeight strikeMediantime = profile "createBlockTimeStrike" $ do
  State{ config = Config{ configBlockTimeStrikeMinimumBlockAheadCurrentTip = configBlockTimeStrikeMinimumBlockAheadCurrentTip}
       , blockTimeState = BlockTime.State{ latestConfirmedBlock = latestConfirmedBlockV }
       } <- ask
  mlatestConfirmedBlock <- liftIO $ TVar.readTVarIO latestConfirmedBlockV
  case mlatestConfirmedBlock of
    Nothing -> do
      let err = "ERROR: createBlockTimeStrike: there is no current tip yet"
      runLogging $ $(logError) err
      throwJSON err400 err
    Just tip
      | blockHeaderMediantime tip > fromIntegral strikeMediantime -> do
        let err = "ERROR: strikeMediantime is in the past, which is not expected"
        runLogging $ $(logError) err
        throwJSON err400 err
    Just tip
      | blockHeaderHeight tip + naturalFromPositive configBlockTimeStrikeMinimumBlockAheadCurrentTip > blockHeight -> do
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
      throwJSON err500 ("something went wrong" :: Text)
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
    observeStrikes confirmedBlock = profile "moveFutureStrikesToPastStrikes" $ do
      configBlockspanURL <- asks (configBlockspanURL . config)
      recordsPerReply <- asks (configRecordsPerReply . config)
      state <- ask
      _ <- withDBTransaction "byBlock" $ do
        C.runConduit
          $ streamEntities
            [ BlockTimeStrikeBlock <=. blockHeaderHeight confirmedBlock
            , BlockTimeStrikeObservedBlockHash ==. Nothing
            ]
            BlockTimeStrikeId
            (PageSize (fromPositive recordsPerReply))
            Descend
            (Range Nothing Nothing)
          .| ( do
              let
                  loop (cnt::Int) = do
                    mv <- C.await
                    case mv of
                      Nothing -> do
                        liftIO $ runLoggingIO state $ $(logDebug) ("calculated results for " <> tshow cnt <> " strikes by observing block")
                      Just (Entity strikeId strike) -> do
                        blockHeader <-
                          if blockHeaderHeight confirmedBlock == blockTimeStrikeBlock strike
                          then return confirmedBlock -- if we already have this block header
                          else liftIO $! Blockspan.withClient configBlockspanURL $ getBlockByHeight (blockTimeStrikeBlock strike)
                        when (blockTimeStrikeObservedResult strike == Nothing) $ do -- when haven't been calculated yet
                          lift $ update strikeId
                            [ BlockTimeStrikeObservedResult =.
                              ( Just $!
                                  if fromIntegral (blockHeaderMediantime blockHeader) <= blockTimeStrikeStrikeMediantime strike
                                  then Fast
                                  else Slow
                              )
                            ]
                        lift $ update strikeId
                          [ BlockTimeStrikeObservedBlockMediantime =. (Just $! fromIntegral $ blockHeaderMediantime blockHeader)
                          , BlockTimeStrikeObservedBlockHash =. (Just $! blockHeaderHash blockHeader)
                          ]
                        loop $! (cnt + 1)
              loop 0
            )
      blockTimeStrikeMinimumBlockAheadCurrentTip <- asks (configBlockTimeStrikeMinimumBlockAheadCurrentTip . config)
      _ <- withDBTransaction "byTime" $ do
        C.runConduit
          $ streamEntities
            [ BlockTimeStrikeStrikeMediantime <=. (fromIntegral $ blockHeaderMediantime confirmedBlock)
            , BlockTimeStrikeBlock >. blockHeaderHeight confirmedBlock + naturalFromPositive blockTimeStrikeMinimumBlockAheadCurrentTip -- don't resolve blocks, that had already discovered, but haven't been confirmed yet. It which will be observed soon and the result will be calculated by it's mediantime
            , BlockTimeStrikeObservedResult ==. Nothing
            ]
            BlockTimeStrikeId
            (PageSize (fromPositive recordsPerReply))
            Descend
            (Range Nothing Nothing)
          .| ( do
              let
                  loop (cnt::Int) = do
                    mv <- C.await
                    case mv of
                      Nothing -> do
                        liftIO $ runLoggingIO state $ $(logDebug) ("calculated results for " <> tshow cnt <> " strikes by reaching time")
                      Just (Entity strikeId _) -> do
                        lift $ update strikeId -- we only get result, as no block had been observed yet
                          [ BlockTimeStrikeObservedResult =. Just Slow
                          ]
                        loop $! (cnt + 1)
              loop 0
            )
      return ()

-- | returns list of BlockTimeStrikePublic records
getBlockTimeStrikesPage
  :: Maybe (Natural Int)
  -> Maybe (FilterRequest BlockTimeStrike BlockTimeStrikeFilter)
  -> AppM (PagingResult BlockTimeStrikePublic)
getBlockTimeStrikesPage mpage mfilter = profile "getBlockTimeStrikesPage" $ do
  latestConfirmedBlockV <- asks (BlockTime.latestConfirmedBlock . blockTimeState)
  mlatestConfirmedBlock <- liftIO $ TVar.readTVarIO latestConfirmedBlockV
  averageBlockDiscoverSecs <- asks (configAverageBlockDiscoverSecs . config)
  minimumBlocksAhead <- asks (configBlockTimeStrikeMinimumBlockAheadCurrentTip . config)
  case mlatestConfirmedBlock of
    Nothing -> do
      let msg = "getBlockTimeStrikesPage: no confirmed block found yet"
      runLogging $  $(logError) msg
      throwJSON err500 msg
    Just confirmedBlock -> do
      let finalFilter =
            case maybe Nothing (blockTimeStrikeFilterClass . fst . unFilterRequest) mfilter of
              Nothing -> filter
              Just BlockTimeStrikeFilterClassGuessable ->
                ( (BlockTimeStrikeBlock >=. blockHeaderHeight confirmedBlock + naturalFromPositive minimumBlocksAhead)
                 :(BlockTimeStrikeStrikeMediantime >=. (fromIntegral (blockHeaderMediantime confirmedBlock) + (fromIntegral $ minimumBlocksAhead * averageBlockDiscoverSecs)))
                 :filter
                )
              Just BlockTimeStrikeFilterClassOutcomeKnown ->
                ( (BlockTimeStrikeObservedResult !=. Nothing)
                 :filter
                )
              Just BlockTimeStrikeFilterClassOutcomeUnknown ->
                ( (BlockTimeStrikeObservedResult ==. Nothing)
                 :filter
                )
      mret <- getBlockTimeStrikePast finalFilter
      case mret of
        Nothing -> do
          throwJSON err500 ("something went wrong"::Text)
        Just ret -> return ret
  where
    sort = maybe Descend (sortOrder . unFilterRequest) mfilter
    filter = (maybe [] (buildFilter . unFilterRequest) mfilter)
    getBlockTimeStrikePast finalFilter = do
      pagingResult mpage finalFilter sort BlockTimeStrikeId
        $ ( C.awaitForever $ \(Entity strikeId strike) -> do
            resultsCnt <- lift $ count [ BlockTimeStrikeGuessStrike ==. strikeId ]
            C.yield (BlockTimeStrikePublic {blockTimeStrikePublicStrike = strike, blockTimeStrikePublicGuessesCount = fromIntegral resultsCnt})
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
                         return ( Just (BlockTimeStrikePublic {blockTimeStrikePublicStrike = strike, blockTimeStrikePublicGuessesCount = fromIntegral resultsCnt}))
                       Nothing -> return acc
               in loop Nothing
             )
