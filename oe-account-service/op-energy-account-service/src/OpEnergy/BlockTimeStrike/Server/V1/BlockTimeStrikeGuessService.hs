{-- | This module implements BlockTime strike service.
 -}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards          #-}
module OpEnergy.BlockTimeStrike.Server.V1.BlockTimeStrikeGuessService
  ( getBlockTimeStrikeFutureGuesses
  , createBlockTimeStrikeFutureGuess
  , getBlockTimeStrikeGuessResults
  , getBlockTimeStrikeGuessResultsPage
  , calculateResultsLoop
  , getBlockTimeStrikeFutureGuessesPage
  ) where

import qualified Data.Text as Text
import           Servant (err404, err500, throwError, errBody)
import           Control.Monad.Trans.Reader (ask)
import           Control.Monad.Logger(logDebug, logError)
import           Data.Time.Clock(getCurrentTime)
import           Data.Time.Clock.POSIX(utcTimeToPOSIXSeconds)
import           Data.Maybe(fromJust)
import qualified Control.Concurrent.STM.TVar as TVar
import           Control.Monad(forM, forM_, forever)
import           Control.Concurrent(threadDelay)
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text.Encoding as Text

import           Data.Text.Show(tshow)
import qualified Data.Conduit as C
import           Control.Monad.Trans
import           Database.Persist.Postgresql
import           Prometheus(MonadMonitor)
import qualified Prometheus as P
import           Control.Exception.Safe as E


import           Data.OpEnergy.Account.API.V1.Account
import           Data.OpEnergy.API.V1.Positive( naturalFromPositive)
import           Data.OpEnergy.API.V1.Block
import           Data.OpEnergy.API.V1.Natural
import           Data.OpEnergy.Account.API.V1.BlockTimeStrike
import           Data.OpEnergy.Account.API.V1.BlockTimeStrikeGuess
import           Data.OpEnergy.Account.API.V1.PagingResult
import           Data.OpEnergy.Account.API.V1.FilterRequest
import           OpEnergy.Account.Server.V1.Config (Config(..))
import           OpEnergy.Account.Server.V1.Class (runAppT, AppT, AppM, State(..), runLogging)
import qualified OpEnergy.BlockTimeStrike.Server.V1.Class as BlockTime
import qualified OpEnergy.Account.Server.V1.Metrics as Metrics(MetricsState(..))
import           OpEnergy.Account.Server.V1.AccountService (mgetPersonByAccountToken)
import           OpEnergy.PagingResult

-- | O(ln users) + O(strike future)
-- returns list BlockTimeStrikeFuture records. requires authenticated user
getBlockTimeStrikeFutureGuesses :: AccountToken-> BlockHeight-> Natural Int-> AppM [BlockTimeStrikeGuessPublic ]
getBlockTimeStrikeFutureGuesses token blockHeight nlocktime = do
  mperson <- mgetPersonByAccountToken token
  case mperson of
    Nothing -> do
      let err = "ERROR: getBlockTimeStrikeFutureGuesses: failed to authenticate user"
      runLogging $ $(logError) err
      throwError err404 {errBody = BS.fromStrict (Text.encodeUtf8 err)}
    Just person -> do
      mret <- getBlockTimeStrikeFutureGuesses person
      case mret of
        Nothing-> do
          let err = "ERROR: getBlockTimeStrikeFutureGuesses: failed to get future strike for a given user"
          runLogging $ $(logError) err
          throwError err404 {errBody = BS.fromStrict (Text.encodeUtf8 err)}
        Just ret-> return ret
  where
    getBlockTimeStrikeFutureGuesses :: (MonadIO m, MonadMonitor m) => Entity Person -> AppT m (Maybe [BlockTimeStrikeGuessPublic])
    getBlockTimeStrikeFutureGuesses (Entity _ _) = do
      State{ accountDBPool = pool
           , metrics = Metrics.MetricsState { Metrics.getBlockTimeStrikeFutureGuesses = getBlockTimeStrikeFutureGuesses
                                            }
           } <- ask
      P.observeDuration getBlockTimeStrikeFutureGuesses $ do
        liftIO $ flip runSqlPersistMPool pool $ do
          mstrike <- selectFirst [ BlockTimeStrikeFutureBlock ==. blockHeight
                                 , BlockTimeStrikeFutureNlocktime ==. fromIntegral nlocktime
                                 ] []
          case mstrike of
            Nothing-> return Nothing
            Just (Entity strikeKey strike) -> do
              ret <- selectList [ BlockTimeStrikeFutureGuessStrike ==. strikeKey] []
              mapM (\(Entity _ guess) -> do
                       person <- get (blockTimeStrikeFutureGuessPerson guess) >>= return . fromJust
                       return $! BlockTimeStrikeGuessPublic
                         { person = personUuid person
                         , strike = strike
                         , creationTime = blockTimeStrikeFutureGuessCreationTime guess
                         , guess = blockTimeStrikeFutureGuessGuess guess
                         }
                   ) ret >>= return . Just

-- | O(ln users) + O(strike future)
-- returns list BlockTimeStrikeFuture records
getBlockTimeStrikeFutureGuessesPage
  :: BlockHeight
  -> Natural Int
  -> Maybe (Natural Int)
  -> Maybe (FilterRequest BlockTimeStrikeFutureGuess BlockTimeStrikeGuessPublicFilter)
  -> AppM (PagingResult BlockTimeStrikeGuessPublic)
getBlockTimeStrikeFutureGuessesPage blockHeight nlocktime mpage mfilter = do
  State{ metrics = Metrics.MetricsState { Metrics.getBlockTimeStrikeFutureGuesses = getBlockTimeStrikeFutureGuesses
                                        }
        } <- ask
  P.observeDuration getBlockTimeStrikeFutureGuesses $ do
    mstrike <- mgetBlockTimeStrikeFuture blockHeight nlocktime
    case mstrike of
      Nothing -> do
        let err = "getBlockTimeStrikeFutureGuessesPage: no blocktime strike future found"
        runLogging $ $(logError) err
        throwError err404 {errBody = BS.fromStrict (Text.encodeUtf8 err)}
      Just (Entity strikeId strike) -> do
        eret <- pagingResult
          mpage
          ([ BlockTimeStrikeFutureGuessStrike ==. strikeId ] ++ maybe [] (buildFilter . unFilterRequest) mfilter
          ) BlockTimeStrikeFutureGuessCreationTime
          $ ( C.awaitForever $ \(Entity _ guess) -> do
              person <- lift $ get (blockTimeStrikeFutureGuessPerson guess) >>= return . fromJust
              C.yield $! BlockTimeStrikeGuessPublic
                        { person = personUuid person
                        , strike = strike
                        , creationTime = blockTimeStrikeFutureGuessCreationTime guess
                        , guess = blockTimeStrikeFutureGuessGuess guess
                        }
            )
        case eret of
          Left some -> do
            let err = "getBlockTimeStrikeFutureGuessesPage: " <> Text.pack (show some)
            runLogging $ $(logError) err
            throwError err500 {errBody = BS.fromStrict (Text.encodeUtf8 err)}
          Right ret-> return ret

mgetBlockTimeStrikeFuture :: (MonadIO m, MonadMonitor m) => BlockHeight-> Natural Int-> AppT m (Maybe (Entity BlockTimeStrikeFuture))
mgetBlockTimeStrikeFuture blockHeight nlocktime = do
  State{ accountDBPool = pool
       , metrics = Metrics.MetricsState { Metrics.mgetBlockTimeStrikeFuture = mgetBlockTimeStrikeFuture
                                        }
       } <- ask
  P.observeDuration mgetBlockTimeStrikeFuture $ do
    liftIO $ flip runSqlPersistMPool pool $ do
      mstrike <- selectFirst [ BlockTimeStrikeFutureBlock ==. blockHeight
                             , BlockTimeStrikeFutureNlocktime ==. fromIntegral nlocktime
                             ] []
      case mstrike of
        Nothing-> return Nothing
        Just strike -> return (Just strike)

mgetBlockTimeStrikePast :: (MonadIO m, MonadMonitor m) => BlockHeight-> Natural Int-> AppT m (Maybe (Entity BlockTimeStrikePast))
mgetBlockTimeStrikePast blockHeight nlocktime = do
  State{ accountDBPool = pool
       , metrics = Metrics.MetricsState { Metrics.mgetBlockTimeStrikePast = mgetBlockTimeStrikePast
                                        }
       } <- ask
  P.observeDuration mgetBlockTimeStrikePast $ do
    liftIO $ flip runSqlPersistMPool pool $ do
      mstrike <- selectFirst [ BlockTimeStrikePastBlock ==. blockHeight
                             , BlockTimeStrikePastNlocktime ==. fromIntegral nlocktime
                             ] []
      case mstrike of
        Nothing-> return Nothing
        Just strike -> return (Just strike)

-- | O(ln accounts).
-- Tries to create future block time strike. Requires authenticated user and blockheight should be in the future
createBlockTimeStrikeFutureGuess :: AccountToken-> BlockHeight-> Natural Int-> SlowFast-> AppM ()
createBlockTimeStrikeFutureGuess token blockHeight nlocktime guess = do
  State{ config = Config{ configBlockTimeStrikeFutureGuessMinimumBlockAheadCurrentTip = configBlockTimeStrikeFutureGuessMinimumBlockAheadCurrentTip
                        }
       , blockTimeState = BlockTime.State{ latestConfirmedBlock = latestConfirmedBlockV }
       } <- ask
  mlatestConfirmedBlock <- liftIO $ TVar.readTVarIO latestConfirmedBlockV
  case mlatestConfirmedBlock of
    Nothing -> do
      let err = "ERROR: createBlockTimeStrikeFutureGuess: there is no current tip yet"
      runLogging $ $(logError) err
      throwError err404 {errBody = BS.fromStrict (Text.encodeUtf8 err)}
    Just tip
      | blockHeaderMediantime tip > fromIntegral nlocktime -> do
        let err = "ERROR: createBlockTimeStrikeFutureGuess: nlocktime is in the past, which is not expected"
        runLogging $ $(logError) err
        throwError err404 {errBody = BS.fromStrict (Text.encodeUtf8 err)}
    Just tip
      | blockHeaderHeight tip + naturalFromPositive configBlockTimeStrikeFutureGuessMinimumBlockAheadCurrentTip > blockHeight -> do
        let err = "ERROR: createBlockTimeStrikeFutureGuess: block height for new block time strike should be in the future + minimum configBlockTimeStrikeFutureGuessMinimumBlockAheadCurrentTip"
        runLogging $ $(logError) err
        throwError err404 {errBody = BS.fromStrict (Text.encodeUtf8 err)}
    _ -> do
      mperson <- mgetPersonByAccountToken token
      case mperson of
        Nothing-> do
          let err = "ERROR: createBlockTimeStrikeFutureGuess: person was not able to authenticate itself"
          runLogging $ $(logError) err
          throwError err404 {errBody = BS.fromStrict (Text.encodeUtf8 err)}
        Just (Entity personKey _) -> do
          mstrike <- mgetBlockTimeStrikeFuture blockHeight nlocktime
          case mstrike of
            Nothing-> do
              let err = "ERROR: createBlockTimeStrikeFutureGuess: future strike was not able to authenticate itself"
              runLogging $ $(logError) err
              throwError err404 {errBody = BS.fromStrict (Text.encodeUtf8 err)}
            Just (Entity strikeKey _) -> do
              createBlockTimeStrikeFutureGuess personKey strikeKey guess
  where
    createBlockTimeStrikeFutureGuess :: (MonadMonitor m, MonadIO m) => Key Person-> Key BlockTimeStrikeFuture-> SlowFast-> AppT m ()
    createBlockTimeStrikeFutureGuess personKey strikeKey guess = do
      State{ metrics = Metrics.MetricsState { Metrics.createBlockTimeStrikeFutureGuess = createBlockTimeStrikeFutureGuess
                                            }
           , accountDBPool = pool
           } <- ask
      P.observeDuration createBlockTimeStrikeFutureGuess $ do
        nowUTC <- liftIO getCurrentTime
        let now = utcTimeToPOSIXSeconds nowUTC
        _ <- liftIO $ flip runSqlPersistMPool pool $ insert $ BlockTimeStrikeFutureGuess
          { blockTimeStrikeFutureGuessGuess = guess
          , blockTimeStrikeFutureGuessCreationTime = now
          , blockTimeStrikeFutureGuessPerson = personKey
          , blockTimeStrikeFutureGuessStrike = strikeKey
          }
        return ()

-- | O(ln accounts) + O(BlockTimeStrikePast).
-- returns list BlockTimeStrikePast records. requires authenticated user
getBlockTimeStrikeGuessResults :: AccountToken-> BlockHeight-> Natural Int-> AppM [ BlockTimeStrikeGuessResultPublic]
getBlockTimeStrikeGuessResults token blockHeight nlocktime = do
  mperson <- mgetPersonByAccountToken token
  case mperson of
    Just person -> do
      mstrike <- mgetBlockTimeStrikePast blockHeight nlocktime
      case mstrike of
        Just strike-> getBlockTimeStrikeGuessResult person strike
        Nothing-> do
          let err = "ERROR: getBlockTimeStrikeGuessResults: there is no past blocktime strike with given block height and nlocktime"
          runLogging $ $(logError) err
          throwError err404 {errBody = BS.fromStrict (Text.encodeUtf8 err)}
    Nothing -> do
      let msg = "ERROR: getBlockTimeStrikeGuessResults: person was not able to authenticate itself"
      runLogging $ $(logError) msg
      throwError err404
  where
    getBlockTimeStrikeGuessResult :: (MonadIO m, MonadMonitor m) => (Entity Person) -> Entity BlockTimeStrikePast-> AppT m [ BlockTimeStrikeGuessResultPublic]
    getBlockTimeStrikeGuessResult _ (Entity _ strike) = do
      State{ accountDBPool = pool
           , metrics = Metrics.MetricsState { Metrics.getBlockTimeStrikePast = getBlockTimeStrikePast
                                            }
           } <- ask
      P.observeDuration getBlockTimeStrikePast $ do
        liftIO $ flip runSqlPersistMPool pool $ do
          (guessResults :: [Entity BlockTimeStrikePastGuess]) <- selectList [] []
          forM guessResults $ \(Entity _ guessResult)-> do
            person <- get (blockTimeStrikePastGuessPerson guessResult) >>= return . fromJust -- persistent ensures that person with appropriate PersonId exist
            return $ BlockTimeStrikeGuessResultPublic
              { person = personUuid person
              , strike = strike
              , creationTime = blockTimeStrikePastGuessFutureGuessCreationTime guessResult
              , archiveTime = blockTimeStrikePastGuessCreationTime guessResult
              , guess = blockTimeStrikePastGuessGuess guessResult
              , observedResult = blockTimeStrikePastGuessObservedResult guessResult
              }

-- | O(ln accounts) + O(BlockTimeStrikePast).
-- returns list BlockTimeStrikePast records
getBlockTimeStrikeGuessResultsPage
  :: BlockHeight
  -> Natural Int
  -> Maybe (Natural Int)
  -> Maybe (FilterRequest BlockTimeStrikePastGuess BlockTimeStrikeGuessResultPublicFilter)
  -> AppM (PagingResult BlockTimeStrikeGuessResultPublic)
getBlockTimeStrikeGuessResultsPage blockHeight nlocktime mpage mfilter = do
  mblock <- mgetBlockTimeStrikePast blockHeight nlocktime
  case mblock of
    Nothing -> do
      let err = "ERROR: getBlockTimeStrikeGuessResultsPage: strike not found"
      runLogging $ $(logError) err
      throwError err404 {errBody = BS.fromStrict (Text.encodeUtf8 err)}
    Just pastE-> do
      eret <- getBlockTimeStrikeGuessResults pastE
      case eret of
        Left some -> do
          let err = "ERROR: getBlockTimeStrikeGuessResultsPage: " <> Text.pack (show some)
          runLogging $ $(logError) err
          throwError err500 {errBody = BS.fromStrict (Text.encodeUtf8 err)}
        Right ret -> return ret
  where
    getBlockTimeStrikeGuessResults :: (MonadIO m, MonadMonitor m, MonadCatch m) => Entity BlockTimeStrikePast-> AppT m (Either SomeException (PagingResult BlockTimeStrikeGuessResultPublic))
    getBlockTimeStrikeGuessResults (Entity pastId past) = do
      State{ metrics = Metrics.MetricsState { Metrics.getBlockTimeStrikeFuture = getBlockTimeStrikeFuture
                                            }
           } <- ask
      P.observeDuration getBlockTimeStrikeFuture $ do
        pagingResult mpage
          ([ BlockTimeStrikePastGuessStrike ==. pastId ] ++ maybe [] (buildFilter . unFilterRequest) mfilter
          ) BlockTimeStrikePastGuessCreationTime
          $ (C.awaitForever $ \(Entity _ guessResult) -> do
              person <- lift $ get (blockTimeStrikePastGuessPerson guessResult) >>= return . fromJust -- persistent ensures that person with appropriate PersonId exist
              C.yield $ BlockTimeStrikeGuessResultPublic
                { person = personUuid person
                , strike = past
                , creationTime = blockTimeStrikePastGuessFutureGuessCreationTime guessResult
                , archiveTime = blockTimeStrikePastGuessCreationTime guessResult
                , guess = blockTimeStrikePastGuessGuess guessResult
                , observedResult = blockTimeStrikePastGuessObservedResult guessResult
                }
            )

calculateResultsLoop :: (MonadIO m, MonadMonitor m) => AppT m ()
calculateResultsLoop = forever $ do
  State{ config = Config {configSchedulerPollRateSecs = configSchedulerPollRateSecs}} <- ask
  runLogging $ $(logDebug) $ "calculateResultsLoop: iteration"
  calculateResultsIteration
  liftIO $ threadDelay $ 1000000 * (fromIntegral configSchedulerPollRateSecs) -- sleep for poll rate secs
  where
     calculateResultsIteration :: (MonadIO m, MonadMonitor m) => AppT m ()
     calculateResultsIteration = do
      state@State{ accountDBPool = pool
                 } <- ask
      liftIO $ flip runSqlPersistMPool pool $ do
        (observedBlocks :: [Entity BlockTimeStrikeFutureObservedBlock]) <- selectList [] []
        forM_ observedBlocks $ \(Entity observedBlockKey observedBlock) -> do
          nowUTC <- liftIO getCurrentTime
          let now = utcTimeToPOSIXSeconds nowUTC
          -- get past strike
          pastStrike <- get (blockTimeStrikeFutureObservedBlockPastStrike observedBlock) >>= return . fromJust
          futureStrike <- get (blockTimeStrikeFutureObservedBlockFutureStrike observedBlock) >>= return . fromJust
          if blockTimeStrikePastBlock pastStrike == blockTimeStrikeFutureBlock futureStrike
            then do
              -- get guesses
              (guesses:: [Entity BlockTimeStrikeFutureGuess]) <- selectList [ BlockTimeStrikeFutureGuessStrike ==. (blockTimeStrikeFutureObservedBlockFutureStrike observedBlock)] []
              -- create results, removing guesses
              _ <- forM_ guesses $ \(Entity guessId guess) -> do
                -- ideally we should notify user in some way about "you were wrong/right!" message
                -- create result
                _ <- insert $ BlockTimeStrikePastGuess
                  { blockTimeStrikePastGuessObservedResult = blockTimeStrikePastObservedResult pastStrike
                  , blockTimeStrikePastGuessGuess = blockTimeStrikeFutureGuessGuess guess
                  , blockTimeStrikePastGuessCreationTime = now
                  , blockTimeStrikePastGuessFutureGuessCreationTime = blockTimeStrikeFutureGuessCreationTime guess
                  , blockTimeStrikePastGuessStrike = blockTimeStrikeFutureObservedBlockPastStrike observedBlock
                  , blockTimeStrikePastGuessPerson = blockTimeStrikeFutureGuessPerson guess
                  }
                -- remove guess
                delete guessId
              -- remove observedBlock
              _ <- delete observedBlockKey
              liftIO $ runAppT state $ do
                runLogging $ $(logDebug) $ "calculateResultsLoop: removed observedBlockKey "
                  <> tshow (blockTimeStrikePastBlock pastStrike) <> " , "
                  <> tshow (blockTimeStrikeFutureBlock futureStrike)
                  <> ", observedBlockKey : " <> tshow observedBlockKey
              -- mark blocktime strike future as ready to be removed
              _ <- insert $ BlockTimeStrikeFutureReadyToRemove
                { blockTimeStrikeFutureReadyToRemoveFutureStrike = (blockTimeStrikeFutureObservedBlockFutureStrike observedBlock)
                , blockTimeStrikeFutureReadyToRemovePastStrike = (blockTimeStrikeFutureObservedBlockPastStrike observedBlock)
                }
              return ()
            else do -- move observed block to observed block fail
              liftIO $ runAppT state $ do
                runLogging $ $(logError) $ "calculateResultsLoop: BlockTimeStrikePastBlock != BlockTimeStrikeFutureBlock: "
                  <> tshow (blockTimeStrikePastBlock pastStrike) <> " != "
                  <> tshow (blockTimeStrikeFutureBlock futureStrike)
                  <> ", observedBlockKey : " <> tshow observedBlockKey
                  <> ", this is not expected, deleting this record: it should be properly recreated when the next current tip will be discovered or during boot"
              delete observedBlockKey
