{-- | This module implements BlockTime strike service.
 -}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE FlexibleContexts          #-}
module OpEnergy.BlockTimeStrike.Server.V1.BlockTimeStrikeGuessService
  ( getBlockTimeStrikeFutureGuesses
  , createBlockTimeStrikeFutureGuess
  , getBlockTimeStrikeGuessResults
  , getBlockTimeStrikeGuessResultsPage
  , calculateResultsLoop
  , getBlockTimeStrikeFutureGuessesPage
  ) where

import           Servant (err400, err404, err500, throwError, errBody)
import           Control.Monad.Trans.Reader (ask)
import           Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import           Control.Monad.Logger(logDebug, logError)
import           Data.Time.Clock(getCurrentTime)
import           Data.Time.Clock.POSIX(utcTimeToPOSIXSeconds)
import           Data.Maybe(fromJust)
import qualified Control.Concurrent.STM.TVar as TVar
import           Control.Monad(forM, forM_, forever, void)
import           Control.Concurrent(threadDelay)
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text.Encoding as Text

import           Data.Text.Show(tshow)
import           Data.Conduit ((.|))
import qualified Data.Conduit as C
import qualified Data.Conduit.Internal as C(zipSources)
import           Control.Monad.Trans
import           Database.Persist.Postgresql
import           Database.Persist.Pagination
import           Prometheus(MonadMonitor)
import           Control.Exception.Safe as E


import           Data.OpEnergy.Account.API.V1.Account
import           Data.OpEnergy.API.V1.Positive( naturalFromPositive)
import           Data.OpEnergy.API.V1.Block
import           Data.OpEnergy.API.V1.Natural
import           Data.OpEnergy.API.V1.Positive(fromPositive)
import           Data.OpEnergy.Account.API.V1.BlockTimeStrike
import           Data.OpEnergy.Account.API.V1.BlockTimeStrikeGuess
import           Data.OpEnergy.Account.API.V1.PagingResult
import           Data.OpEnergy.Account.API.V1.FilterRequest
import           OpEnergy.Account.Server.V1.Config (Config(..))
import           OpEnergy.Account.Server.V1.Class (runAppT, AppT, AppM, State(..), runLogging, profile, withDBTransaction)
import qualified OpEnergy.BlockTimeStrike.Server.V1.Class as BlockTime
import           OpEnergy.Account.Server.V1.AccountService (mgetPersonByAccountToken)
import           OpEnergy.PagingResult

-- | O(ln users) + O(strike future)
-- returns list BlockTimeStrikeFuture records. requires authenticated user
getBlockTimeStrikeFutureGuesses :: AccountToken-> BlockHeight-> Natural Int-> AppM [BlockTimeStrikeGuessPublic ]
getBlockTimeStrikeFutureGuesses token blockHeight nlocktime = profile "getBlockTimeStrikeFutureGuesses" $ do
  mperson <- mgetPersonByAccountToken token
  case mperson of
    Nothing -> do
      let err = "ERROR: getBlockTimeStrikeFutureGuesses: failed to authenticate user"
      runLogging $ $(logError) err
      throwError err404 {errBody = BS.fromStrict (Text.encodeUtf8 err)}
    Just person -> do
      mret <- getBlockTimeStrikeFutureGuesses person
      case mret of
        Nothing -> throwError err500 { errBody = "something went wrong"}
        Just Nothing -> do
          let err = "ERROR: getBlockTimeStrikeFutureGuesses: failed to get future strike for a given user"
          runLogging $ $(logError) err
          throwError err404 {errBody = BS.fromStrict (Text.encodeUtf8 err)}
        Just (Just ret)-> return ret
  where
    getBlockTimeStrikeFutureGuesses :: (MonadIO m, MonadMonitor m) => Entity Person -> AppT m (Maybe (Maybe [BlockTimeStrikeGuessPublic]))
    getBlockTimeStrikeFutureGuesses (Entity _ _) = profile "getBlockTimeStrikeFutureGuesses" $ do
      withDBTransaction "" $ runMaybeT $ do
        Entity strikeKey strike <- MaybeT $ selectFirst [ BlockTimeStrikeFutureBlock ==. blockHeight
                               , BlockTimeStrikeFutureNlocktime ==. fromIntegral nlocktime
                               ] []
        ret <- MaybeT $ Just <$> selectList [ BlockTimeStrikeFutureGuessStrike ==. strikeKey] []
        MaybeT $ Just <$> mapM (\(Entity _ guess) -> do
                 person <- get (blockTimeStrikeFutureGuessPerson guess) >>= return . fromJust
                 return $! BlockTimeStrikeGuessPublic
                   { person = personUuid person
                   , strike = strike
                   , creationTime = blockTimeStrikeFutureGuessCreationTime guess
                   , guess = blockTimeStrikeFutureGuessGuess guess
                   }
             ) ret

-- | O(ln users) + O(strike future)
-- returns list BlockTimeStrikeFuture records
getBlockTimeStrikeFutureGuessesPage
  :: Maybe (Natural Int)
  -> Maybe (FilterRequest BlockTimeStrikeFutureGuess BlockTimeStrikeGuessPublicFilter)
  -> AppM (PagingResult BlockTimeStrikeGuessPublic)
getBlockTimeStrikeFutureGuessesPage mpage mfilter = profile "getBlockTimeStrikeFutureGuessesPage" $ do
  State{  config = Config{ configRecordsPerReply = recordsPerReply}
        } <- ask
  mret <- pagingResult mpage (maybe [] (buildFilter . unFilterRequest . mapFilter) mfilter ) sort BlockTimeStrikeFutureCreationTime
    $ ( C.awaitForever $ \v@(Entity futureId _) -> do
        C.toProducer $ C.zipSources
          ( repeatC v
          )
          ( streamEntities
            ( (BlockTimeStrikeFutureGuessStrike ==. futureId) : (maybe [] (buildFilter . unFilterRequest) mfilter))
            BlockTimeStrikeFutureGuessCreationTime
            (PageSize ((fromPositive recordsPerReply) + 1))
            Descend
            (Range Nothing Nothing)
          )
      )
    .| ( C.awaitForever $ \(futureE, guessE@(Entity _ guessResult)) -> do
        C.toProducer $ C.zipSources
          ( repeatC (futureE, guessE)
          )
          ( streamEntities
            ( (PersonId ==. blockTimeStrikeFutureGuessPerson guessResult) : (maybe [] (buildFilter . unFilterRequest . mapFilter) mfilter))
            PersonCreationTime
            (PageSize ((fromPositive recordsPerReply) + 1))
            Descend
            (Range Nothing Nothing)
          )
        )
    .| ( C.awaitForever $ \((Entity _ strike, Entity _ guess), Entity _ person )-> do
        C.yield $ BlockTimeStrikeGuessPublic
                  { person = personUuid person
                  , strike = strike
                  , creationTime = blockTimeStrikeFutureGuessCreationTime guess
                  , guess = blockTimeStrikeFutureGuessGuess guess
                  }
       )
  case mret of
    Nothing -> do
      throwError err500 {errBody = "something went wrong"}
    Just ret-> return ret
  where
    repeatC v = C.yield v >> repeatC v
    sort = maybe Descend (sortOrder . unFilterRequest) mfilter

mgetBlockTimeStrikeFuture :: (MonadIO m, MonadMonitor m) => BlockHeight-> Natural Int-> AppT m (Maybe (Entity BlockTimeStrikeFuture))
mgetBlockTimeStrikeFuture blockHeight nlocktime = profile "mgetBlockTimeStrikeFuture" $ do
  mret <- withDBTransaction "" $ do
    selectFirst [ BlockTimeStrikeFutureBlock ==. blockHeight
                , BlockTimeStrikeFutureNlocktime ==. fromIntegral nlocktime
                ] []
  case mret of
    Just some -> return some
    _ -> return Nothing

mgetBlockTimeStrikePast :: (MonadIO m, MonadMonitor m) => BlockHeight-> Natural Int-> AppT m (Maybe (Entity BlockTimeStrikePast))
mgetBlockTimeStrikePast blockHeight nlocktime = profile "mgetBlockTimeStrikePast" $ do
  mret <- withDBTransaction "" $ do
    selectFirst [ BlockTimeStrikePastBlock ==. blockHeight
                , BlockTimeStrikePastNlocktime ==. fromIntegral nlocktime
                ] []
  case mret of
    Just ret -> return ret
    Nothing -> return Nothing

-- | O(ln accounts).
-- Tries to create future block time strike. Requires authenticated user and blockheight should be in the future
createBlockTimeStrikeFutureGuess :: AccountToken-> BlockHeight-> Natural Int-> SlowFast-> AppM ()
createBlockTimeStrikeFutureGuess token blockHeight nlocktime guess = profile "createBlockTimeStrikeFutureGuess" $ do
  State{ config = Config{ configBlockTimeStrikeFutureGuessMinimumBlockAheadCurrentTip = configBlockTimeStrikeFutureGuessMinimumBlockAheadCurrentTip
                        }
       , blockTimeState = BlockTime.State{ latestConfirmedBlock = latestConfirmedBlockV }
       } <- ask
  mlatestConfirmedBlock <- liftIO $ TVar.readTVarIO latestConfirmedBlockV
  case mlatestConfirmedBlock of
    Nothing -> do
      let err = "ERROR: createBlockTimeStrikeFutureGuess: there is no current tip yet"
      runLogging $ $(logError) err
      throwError err400 {errBody = BS.fromStrict (Text.encodeUtf8 err)}
    Just tip
      | blockHeaderMediantime tip > fromIntegral nlocktime -> do
        let err = "ERROR: createBlockTimeStrikeFutureGuess: nlocktime is in the past, which is not expected"
        runLogging $ $(logError) err
        throwError err400 {errBody = BS.fromStrict (Text.encodeUtf8 err)}
    Just tip
      | blockHeaderHeight tip + naturalFromPositive configBlockTimeStrikeFutureGuessMinimumBlockAheadCurrentTip > blockHeight -> do
        let err = "ERROR: createBlockTimeStrikeFutureGuess: block height for new block time strike should be in the future + minimum configBlockTimeStrikeFutureGuessMinimumBlockAheadCurrentTip"
        runLogging $ $(logError) err
        throwError err400 {errBody = BS.fromStrict (Text.encodeUtf8 err)}
    _ -> do
      mperson <- mgetPersonByAccountToken token
      case mperson of
        Nothing-> do
          let err = "ERROR: createBlockTimeStrikeFutureGuess: person was not able to authenticate itself"
          runLogging $ $(logError) err
          throwError err400 {errBody = BS.fromStrict (Text.encodeUtf8 err)}
        Just (Entity personKey _) -> do
          mstrike <- mgetBlockTimeStrikeFuture blockHeight nlocktime
          case mstrike of
            Nothing-> do
              let err = "ERROR: createBlockTimeStrikeFutureGuess: future strike was not able to authenticate itself"
              runLogging $ $(logError) err
              throwError err400 {errBody = BS.fromStrict (Text.encodeUtf8 err)}
            Just (Entity strikeKey _) -> do
              mret <- createBlockTimeStrikeFutureGuess personKey strikeKey guess
              case mret of
                Nothing -> throwError err500 {errBody = "something went wrong"}
                Just _ -> return ()
  where
    createBlockTimeStrikeFutureGuess :: (MonadMonitor m, MonadIO m) => Key Person-> Key BlockTimeStrikeFuture-> SlowFast-> AppT m (Maybe ())
    createBlockTimeStrikeFutureGuess personKey strikeKey guess = profile "createBlockTimeStrikeFutureGuess" $ do
      nowUTC <- liftIO getCurrentTime
      let now = utcTimeToPOSIXSeconds nowUTC
      withDBTransaction "" $ do
        _ <- insert $ BlockTimeStrikeFutureGuess
          { blockTimeStrikeFutureGuessGuess = guess
          , blockTimeStrikeFutureGuessCreationTime = now
          , blockTimeStrikeFutureGuessPerson = personKey
          , blockTimeStrikeFutureGuessStrike = strikeKey
          }
        return ()

-- | O(ln accounts) + O(BlockTimeStrikePast).
-- returns list BlockTimeStrikePast records. requires authenticated user
getBlockTimeStrikeGuessResults :: AccountToken-> BlockHeight-> Natural Int-> AppM [ BlockTimeStrikeGuessResultPublic]
getBlockTimeStrikeGuessResults token blockHeight nlocktime = profile "getBlockTimeStrikeGuessResults" $ do
  mperson <- mgetPersonByAccountToken token
  case mperson of
    Just person -> do
      mstrike <- mgetBlockTimeStrikePast blockHeight nlocktime
      case mstrike of
        Just strike-> do
          mret <- getBlockTimeStrikeGuessResult person strike
          case mret of
            Nothing -> throwError err500 { errBody = "something went wrong"}
            Just ret -> return ret
        Nothing-> do
          let err = "ERROR: getBlockTimeStrikeGuessResults: there is no past blocktime strike with given block height and nlocktime"
          runLogging $ $(logError) err
          throwError err404 {errBody = BS.fromStrict (Text.encodeUtf8 err)}
    Nothing -> do
      let msg = "ERROR: getBlockTimeStrikeGuessResults: person was not able to authenticate itself"
      runLogging $ $(logError) msg
      throwError err404
  where
    getBlockTimeStrikeGuessResult :: (MonadIO m, MonadMonitor m) => (Entity Person) -> Entity BlockTimeStrikePast-> AppT m (Maybe [ BlockTimeStrikeGuessResultPublic])
    getBlockTimeStrikeGuessResult _ (Entity _ strike) = do
      withDBTransaction "" $ do
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
  :: Maybe (Natural Int)
  -> Maybe (FilterRequest BlockTimeStrikePastGuess BlockTimeStrikeGuessResultPublicFilter)
  -> AppM (PagingResult BlockTimeStrikeGuessResultPublic)
getBlockTimeStrikeGuessResultsPage mpage mfilter = do
  mret <- getBlockTimeStrikeGuessResults
  case mret of
    Nothing -> do
      throwError err500 {errBody = "something went wrong"}
    Just ret -> return ret
  where
    repeatC v = C.yield v >> repeatC v
    sort = maybe Descend (sortOrder . unFilterRequest . id1 . mapFilter) mfilter
      where
        id1 :: FilterRequest BlockTimeStrikePast BlockTimeStrikeGuessResultPublicFilter -> FilterRequest BlockTimeStrikePast BlockTimeStrikeGuessResultPublicFilter
        id1 = id -- helping typechecker
    getBlockTimeStrikeGuessResults :: (MonadIO m, MonadMonitor m, MonadCatch m) => AppT m (Maybe (PagingResult BlockTimeStrikeGuessResultPublic))
    getBlockTimeStrikeGuessResults = do
      State{ config = Config{ configRecordsPerReply = recordsPerReply}
           } <- ask
      pagingResult mpage (maybe [] (buildFilter . unFilterRequest . mapFilter ) mfilter) sort BlockTimeStrikePastObservedBlockMediantime
        $ ( C.awaitForever $ \v@(Entity pastId _)-> do
            C.toProducer $ C.zipSources
              ( repeatC v
              )
              ( streamEntities
                ( (BlockTimeStrikePastGuessStrike ==. pastId) : (maybe [] (buildFilter . unFilterRequest . mapFilter) mfilter))
                BlockTimeStrikePastGuessCreationTime
                (PageSize ((fromPositive recordsPerReply) + 1))
                Descend
                (Range Nothing Nothing)
              )
          )
        .| ( C.awaitForever $ \(pastE, guessE@(Entity _ guessResult)) -> do
            C.toProducer $ C.zipSources
              ( repeatC (pastE, guessE)
              )
              ( streamEntities
                ( (PersonId ==. blockTimeStrikePastGuessPerson guessResult) : (maybe [] (buildFilter . unFilterRequest . mapFilter) mfilter))
                PersonCreationTime
                (PageSize ((fromPositive recordsPerReply) + 1))
                Descend
                (Range Nothing Nothing)
              )
           )
        .| ( C.awaitForever $ \((Entity _ past, Entity _ guessResult), Entity _ person )-> do
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
      state <- ask
      void $ withDBTransaction "" $ do
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
