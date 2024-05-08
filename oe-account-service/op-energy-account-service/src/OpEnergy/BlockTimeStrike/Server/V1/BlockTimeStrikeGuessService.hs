{-- | This module implements BlockTime strike service.
 -}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
module OpEnergy.BlockTimeStrike.Server.V1.BlockTimeStrikeGuessService
  ( createBlockTimeStrikeFutureGuess
  , getBlockTimeStrikeGuessResultsPage
  , calculateResultsLoop
  , getBlockTimeStrikeFutureGuessesPage
  ) where

import           Servant (err400, err500, throwError, errBody)
import           Control.Monad.Trans.Reader (ask, asks)
import           Control.Monad.Logger(logDebug, logError)
import           Data.Time.Clock(getCurrentTime)
import           Data.Time.Clock.POSIX(utcTimeToPOSIXSeconds)
import           Data.Maybe(fromJust)
import qualified Control.Concurrent.STM.TVar as TVar
import           Control.Monad( void)
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
import qualified Control.Concurrent.STM as STM


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
import           OpEnergy.Account.Server.V1.Class ( AppT, AppM, State(..), runLoggingIO, runLogging, profile, withDBTransaction)
import qualified OpEnergy.BlockTimeStrike.Server.V1.Class as BlockTime
import           OpEnergy.Account.Server.V1.AccountService (mgetPersonByAccountToken)
import           OpEnergy.PagingResult

-- | O(ln users) + O(strike future)
-- returns list BlockTimeStrikeFuture records
getBlockTimeStrikeFutureGuessesPage
  :: Maybe (Natural Int)
  -> Maybe (FilterRequest BlockTimeStrikeGuess BlockTimeStrikeGuessPublicFilter)
  -> AppM (PagingResult BlockTimeStrikeGuessPublic)
getBlockTimeStrikeFutureGuessesPage mpage mfilter = profile "getBlockTimeStrikeFutureGuessesPage" $ do
  State{  config = Config{ configRecordsPerReply = recordsPerReply}
        } <- ask
  confirmedBlockV <- asks (BlockTime.latestConfirmedBlock . blockTimeState )
  mconfirmedBlock <- liftIO $ TVar.readTVarIO confirmedBlockV
  case mconfirmedBlock of
    Nothing -> do
      let err = "confirmed block haven't been witnessed yet"
      runLogging $ $(logError) err
      throwError err500 { errBody = BS.fromStrict $ Text.encodeUtf8 err}
    Just confirmedBlock -> do
      let
          filter = (BlockTimeStrikeBlock >. blockHeaderHeight confirmedBlock):(maybe [] (buildFilter . unFilterRequest . mapFilter) mfilter )
      mret <- pagingResult mpage filter sort BlockTimeStrikeCreationTime
        $ ( C.awaitForever $ \v@(Entity futureId _) -> do
            C.toProducer $ C.zipSources
              ( repeatC v
              )
              ( streamEntities
                ( (BlockTimeStrikeGuessStrike ==. futureId) : (maybe [] (buildFilter . unFilterRequest) mfilter))
                BlockTimeStrikeGuessCreationTime
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
                ( (PersonId ==. blockTimeStrikeGuessPerson guessResult) : (maybe [] (buildFilter . unFilterRequest . mapFilter) mfilter))
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
                      , creationTime = blockTimeStrikeGuessCreationTime guess
                      , guess = blockTimeStrikeGuessGuess guess
                      }
          )
      case mret of
        Nothing -> do
          throwError err500 {errBody = "something went wrong, check logs for details"}
        Just ret-> return ret
  where
    repeatC v = C.yield v >> repeatC v
    sort = maybe Descend (sortOrder . unFilterRequest) mfilter

mgetBlockTimeStrikeFuture :: (MonadIO m, MonadMonitor m) => BlockHeight-> Natural Int-> AppT m (Maybe (Entity BlockTimeStrike))
mgetBlockTimeStrikeFuture blockHeight nlocktime = profile "mgetBlockTimeStrikeFuture" $ do
  mret <- withDBTransaction "" $ do
    selectFirst [ BlockTimeStrikeBlock ==. blockHeight
                , BlockTimeStrikeNlocktime ==. fromIntegral nlocktime
                ] []
  case mret of
    Just some -> return some
    _ -> return Nothing

-- | O(ln accounts).
-- Tries to create future block time strike. Requires authenticated user and blockheight should be in the future
createBlockTimeStrikeFutureGuess :: AccountToken-> BlockHeight-> Natural Int-> SlowFast-> AppM ()
createBlockTimeStrikeFutureGuess token blockHeight nlocktime guess = profile "createBlockTimeStrikeFutureGuess" $ do
  configBlockTimeStrikeGuessMinimumBlockAheadCurrentTip <- asks (configBlockTimeStrikeGuessMinimumBlockAheadCurrentTip . config)
  latestConfirmedBlockV <- asks (BlockTime.latestConfirmedBlock . blockTimeState)
  mlatestConfirmedBlock <- liftIO $ TVar.readTVarIO latestConfirmedBlockV
  case mlatestConfirmedBlock of
    Nothing -> do
      let err = "ERROR: createBlockTimeStrikeFutureGuess: there is no current tip yet"
      runLogging $ $(logError) err
      throwError err500 {errBody = BS.fromStrict (Text.encodeUtf8 err)}
    Just tip
        | blockHeaderMediantime tip > fromIntegral nlocktime -> do
        let err = "ERROR: createBlockTimeStrikeFutureGuess: nlocktime is in the past, which is not expected"
        runLogging $ $(logError) err
        throwError err400 {errBody = BS.fromStrict (Text.encodeUtf8 err)}
    Just tip
      | blockHeaderHeight tip + naturalFromPositive configBlockTimeStrikeGuessMinimumBlockAheadCurrentTip > blockHeight -> do
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
            Just (Entity strikeKey strike) -> do
              mret <- createBlockTimeStrikeFutureGuess personKey strikeKey (blockTimeStrikeBlock strike) guess
              case mret of
                Nothing -> throwError err500 {errBody = "something went wrong"}
                Just _ -> return ()
  where
    createBlockTimeStrikeFutureGuess :: (MonadMonitor m, MonadIO m) => Key Person-> Key BlockTimeStrike-> BlockHeight-> SlowFast-> AppT m (Maybe ())
    createBlockTimeStrikeFutureGuess personKey strikeKey blockHeight guess = profile "createBlockTimeStrikeFutureGuess" $ do
      nowUTC <- liftIO getCurrentTime
      let now = utcTimeToPOSIXSeconds nowUTC
      withDBTransaction "" $ do
        _ <- insert $ BlockTimeStrikeGuess
          { blockTimeStrikeGuessGuess = guess
          , blockTimeStrikeGuessCreationTime = now
          , blockTimeStrikeGuessPerson = personKey
          , blockTimeStrikeGuessStrike = strikeKey
          , blockTimeStrikeGuessObservedResult = Nothing
          , blockTimeStrikeGuessBlockHeight = blockHeight
          }
        return ()

-- | O(ln accounts) + O(BlockTimeStrikePast).
-- returns list BlockTimeStrikePast records
getBlockTimeStrikeGuessResultsPage
  :: Maybe (Natural Int)
  -> Maybe (FilterRequest BlockTimeStrikeGuess BlockTimeStrikeGuessResultPublicFilter)
  -> AppM (PagingResult BlockTimeStrikeGuessResultPublic)
getBlockTimeStrikeGuessResultsPage mpage mfilter = profile "getBlockTimeStrikeGuessResultsPage" $ do
  mconfirmedBlockV <- asks ( BlockTime.latestConfirmedBlock . blockTimeState)
  mconfirmedBlock <- liftIO $ TVar.readTVarIO mconfirmedBlockV
  case mconfirmedBlock of
    Nothing -> do
      let err = "ERROR: getBlockTimeStrikeGuessResultsPage: no confirmed block found yet"
      runLogging $ $(logError) err
      throwError err500 {errBody = BS.fromStrict (Text.encodeUtf8 err)}
    Just confirmedBlock -> do
      recordsPerReply <- asks ( configRecordsPerReply . config )
      let
          filter = (BlockTimeStrikeBlock <=. blockHeaderHeight confirmedBlock):(maybe [] (buildFilter . unFilterRequest . mapFilter ) mfilter)
      mret <- pagingResult mpage filter sort BlockTimeStrikeId
        $ ( C.awaitForever $ \v@(Entity pastId _)-> do
            C.toProducer $ C.zipSources
              ( repeatC v
              )
              ( streamEntities
                ( (BlockTimeStrikeGuessStrike ==. pastId) : (maybe [] (buildFilter . unFilterRequest . mapFilter) mfilter))
                BlockTimeStrikeGuessCreationTime
                (PageSize ((fromPositive recordsPerReply) + 1))
                Descend
                (Range Nothing Nothing)
              )
          )
        .| ( C.awaitForever $ \(pastE, guessE@(Entity _ guessResult)) -> do
            case blockTimeStrikeGuessObservedResult guessResult of
              Nothing -> return ()
              Just _ -> do
                C.toProducer $ C.zipSources
                  ( repeatC (pastE, guessE)
                  )
                  ( streamEntities
                    ( (PersonId ==. blockTimeStrikeGuessPerson guessResult) : (maybe [] (buildFilter . unFilterRequest . mapFilter) mfilter))
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
              , creationTime = blockTimeStrikeGuessCreationTime guessResult
              , guess = blockTimeStrikeGuessGuess guessResult
              , observedResult = fromJust $ blockTimeStrikeGuessObservedResult guessResult
              }
          )
      case mret of
        Nothing -> do
          throwError err500 {errBody = "something went wrong"}
        Just ret -> return ret
  where
    repeatC v = C.yield v >> repeatC v
    sort = maybe Descend (sortOrder . unFilterRequest . id1 . mapFilter) mfilter
      where
        id1 :: FilterRequest BlockTimeStrike BlockTimeStrikeGuessResultPublicFilter -> FilterRequest BlockTimeStrike BlockTimeStrikeGuessResultPublicFilter
        id1 = id -- helping typechecker

calculateResultsLoop :: (MonadIO m, MonadMonitor m) => AppT m ()
calculateResultsLoop = calculateResultsLoop1 Nothing
  where
    calculateResultsLoop1 :: (MonadIO m, MonadMonitor m) => Maybe BlockHeader -> AppT m ()
    calculateResultsLoop1 mwitnessedBlock = calculateResultsLoop1 =<< (profile "calculateResultsLoop1" $ do
      confirmedBlockV <- asks (BlockTime.latestConfirmedBlock . blockTimeState )
      mconfirmedBlock <- liftIO $ STM.atomically $ TVar.readTVar confirmedBlockV
      newWitnessedBlock <-
        case (mwitnessedBlock, mconfirmedBlock) of
          ( _, Nothing) -> return mwitnessedBlock
          (Nothing, Just confirmedBlock) -> do
            calculateResultsIteration confirmedBlock
            return (Just confirmedBlock)
          ( Just witnessedBlock, Just confirmedBlock)
            | blockHeaderHeight witnessedBlock < blockHeaderHeight confirmedBlock -> do
                calculateResultsIteration confirmedBlock
                return (Just confirmedBlock)
          _ -> return mwitnessedBlock
      configSchedulerPollRateSecs <- asks (configSchedulerPollRateSecs . config)
      liftIO $ threadDelay $ 1000000 * (fromIntegral configSchedulerPollRateSecs) -- sleep for poll rate secs
      return newWitnessedBlock)
    calculateResultsIteration :: (MonadIO m, MonadMonitor m) => BlockHeader -> AppT m ()
    calculateResultsIteration confirmedBlock = profile "calculateResultsIteration" $ do
      recordsPerReply <- asks (configRecordsPerReply . config)
      state <- ask
      void $ withDBTransaction "" $ do
        C.runConduit
          $ streamEntities
            [ BlockTimeStrikeGuessBlockHeight <=. blockHeaderHeight confirmedBlock
            , BlockTimeStrikeGuessObservedResult ==. Nothing
            ]
            BlockTimeStrikeGuessCreationTime
            (PageSize (fromPositive recordsPerReply))
            Descend
            (Range Nothing Nothing)
          .| ( do
               let
                   loop (cnt::Int) = do
                     mv <- C.await
                     case mv of
                       Nothing -> do
                         liftIO $ runLoggingIO state $ $(logDebug) ("calculated results for " <> tshow cnt <> " guesses")
                       Just (Entity guessId guess) -> do
                         mstrike <- lift $ get (blockTimeStrikeGuessStrike guess)
                         case mstrike of
                           Nothing -> loop cnt
                           Just strike -> do
                             lift $ update guessId
                               [ BlockTimeStrikeGuessObservedResult =. blockTimeStrikeObservedResult strike
                               ]
                             loop $! (cnt + 1)
               loop 0
             )
