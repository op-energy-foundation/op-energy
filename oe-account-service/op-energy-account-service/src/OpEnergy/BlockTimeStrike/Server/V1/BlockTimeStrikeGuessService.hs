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
  , getBlockTimeStrikeFutureGuessesPage
  , getBlockTimeStrikeGuessPage
  ) where

import           Servant (err400, err500, throwError, errBody)
import           Control.Monad.Trans.Reader (asks)
import           Control.Monad.Logger( logError)
import           Data.Time.Clock(getCurrentTime)
import           Data.Time.Clock.POSIX(utcTimeToPOSIXSeconds)
import qualified Control.Concurrent.STM.TVar as TVar
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text.Encoding as Text
import qualified Data.List as List

import           Data.Conduit ((.|))
import qualified Data.Conduit as C
import qualified Data.Conduit.Internal as C(zipSources)
import qualified Data.Conduit.List as C
import           Control.Monad.Trans
import           Database.Persist.Postgresql
import           Database.Persist.Pagination
import           Prometheus(MonadMonitor)


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
import           OpEnergy.Account.Server.V1.Class ( AppT, AppM, State(..), runLogging, profile, withDBTransaction, withDBNOTransactionRO)
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
  confirmedBlockV <- asks (BlockTime.latestConfirmedBlock . blockTimeState )
  mconfirmedBlock <- liftIO $ TVar.readTVarIO confirmedBlockV
  case mconfirmedBlock of
    Nothing -> do
      let err = "confirmed block haven't been witnessed yet"
      runLogging $ $(logError) err
      throwError err500 { errBody = BS.fromStrict $ Text.encodeUtf8 err}
    Just confirmedBlock -> do
      let
          page = maybe 0 id mpage
      recordsPerReply <- asks (configRecordsPerReply . config)
      mret <- withDBNOTransactionRO "" $ do
        count <- C.runConduit
          $ streamEntities
              ((BlockTimeStrikeBlock >. blockHeaderHeight confirmedBlock):(maybe [] (buildFilter . unFilterRequest . mapFilter) mfilter ))
              BlockTimeStrikeId
              (PageSize ((fromPositive recordsPerReply) + 1))
              sort
              (Range Nothing Nothing)
          .| ( do
                 let
                     loop (acc::Int) = do
                       mv <- C.await
                       case mv of
                         Nothing -> return acc
                         Just (Entity strikeId _)-> do
                           guesses <- lift $ count (( BlockTimeStrikeGuessStrike ==. strikeId ):(maybe [] (buildFilter . unFilterRequest . mapFilter) mfilter ))
                           loop (acc + guesses)
                 loop 0
             )
        res <- C.runConduit
          $ streamEntities
              ((BlockTimeStrikeBlock >. blockHeaderHeight confirmedBlock):(maybe [] (buildFilter . unFilterRequest . mapFilter) mfilter ))
              BlockTimeStrikeId
              (PageSize ((fromPositive recordsPerReply) + 1))
              sort
              (Range Nothing Nothing)
          .| ( C.awaitForever $ \v@(Entity strikeId _)-> do
              C.toProducer $ C.zipSources
                (repeatC v)
                (streamEntities
                  (( BlockTimeStrikeGuessStrike ==. strikeId ):(maybe [] (buildFilter . unFilterRequest . mapFilter) mfilter ))
                  BlockTimeStrikeGuessId
                  (PageSize ((fromPositive recordsPerReply) + 1))
                  sort
                  (Range Nothing Nothing)
                )
             )
          .| (C.drop (fromNatural page * fromPositive recordsPerReply) >> C.awaitForever C.yield) -- navigate to page
          .| ( C.awaitForever $ \(Entity _ strike, Entity _ guess) -> do
              mperson <- lift $ get (blockTimeStrikeGuessPerson guess)
              case ( mperson) of
                (Nothing ) -> return ()
                ( Just person) -> do
                  C.yield $ BlockTimeStrikeGuessPublic
                            { person = personUuid person
                            , strike = strike
                            , creationTime = blockTimeStrikeGuessCreationTime guess
                            , guess = blockTimeStrikeGuessGuess guess
                            }
             )
          .| C.take (fromPositive recordsPerReply + 1) -- we take +1 to understand if there is a next page available
        return (count, res)
      case mret of
          Nothing -> do
            throwError err500 {errBody = "something went wrong, check logs for details"}
          Just (count, guessesTail) -> do
            let newPage =
                  if List.length guessesTail > fromPositive recordsPerReply
                  then Just (fromIntegral (fromNatural page + 1))
                  else Nothing
                results = List.take (fromPositive recordsPerReply) guessesTail
            return $ PagingResult
              { pagingResultNextPage = newPage
              , pagingResultCount = fromIntegral count
              , pagingResultResults = results
              }
  where
    sort = maybe Descend (sortOrder . unFilterRequest) mfilter
    repeatC v = C.yield v >> repeatC v

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
            Just (Entity strikeKey _) -> do
              mret <- createBlockTimeStrikeFutureGuess personKey strikeKey guess
              case mret of
                Nothing -> throwError err500 {errBody = "something went wrong"}
                Just _ -> return ()
  where
    createBlockTimeStrikeFutureGuess :: (MonadMonitor m, MonadIO m) => Key Person-> Key BlockTimeStrike-> SlowFast-> AppT m (Maybe ())
    createBlockTimeStrikeFutureGuess personKey strikeKey guess = profile "createBlockTimeStrikeFutureGuess" $ do
      nowUTC <- liftIO getCurrentTime
      let now = utcTimeToPOSIXSeconds nowUTC
      withDBTransaction "" $ do
        _ <- insert $ BlockTimeStrikeGuess
          { blockTimeStrikeGuessGuess = guess
          , blockTimeStrikeGuessCreationTime = now
          , blockTimeStrikeGuessPerson = personKey
          , blockTimeStrikeGuessStrike = strikeKey
          }
        return ()

-- | O(ln accounts) + O(BlockTimeStrikePast).
-- returns list BlockTimeStrikePast records
getBlockTimeStrikeGuessResultsPage
  :: Maybe (Natural Int)
  -> Maybe (FilterRequest BlockTimeStrikeGuess BlockTimeStrikeGuessResultPublicFilter)
  -> AppM (PagingResult BlockTimeStrikeGuessPublic)
getBlockTimeStrikeGuessResultsPage mpage mfilter = profile "getBlockTimeStrikeGuessResultsPage" $ do
  mconfirmedBlockV <- asks ( BlockTime.latestConfirmedBlock . blockTimeState)
  mconfirmedBlock <- liftIO $ TVar.readTVarIO mconfirmedBlockV
  case mconfirmedBlock of
    Nothing -> do
      let err = "ERROR: getBlockTimeStrikeGuessResultsPage: no confirmed block found yet"
      runLogging $ $(logError) err
      throwError err500 {errBody = BS.fromStrict (Text.encodeUtf8 err)}
    Just confirmedBlock -> do
      let
          page = maybe 0 id mpage
      recordsPerReply <- asks (configRecordsPerReply . config)
      mret <- withDBNOTransactionRO "" $ do
        count <- C.runConduit
          $ streamEntities
              ((BlockTimeStrikeBlock <=. blockHeaderHeight confirmedBlock):(maybe [] (buildFilter . unFilterRequest . mapFilter) mfilter ))
              BlockTimeStrikeId
              (PageSize ((fromPositive recordsPerReply) + 1))
              sort
              (Range Nothing Nothing)
          .| ( do
                 let
                     loop (acc::Int) = do
                       mv <- C.await
                       case mv of
                         Nothing -> return acc
                         Just (Entity strikeId _)-> do
                           guesses <- lift $ count (( BlockTimeStrikeGuessStrike ==. strikeId ):(maybe [] (buildFilter . unFilterRequest . mapFilter) mfilter ))
                           loop (acc + guesses)
                 loop 0
             )
        res <- C.runConduit
          $ streamEntities
              ((BlockTimeStrikeBlock <=. blockHeaderHeight confirmedBlock):(maybe [] (buildFilter . unFilterRequest . mapFilter) mfilter ))
              BlockTimeStrikeId
              (PageSize ((fromPositive recordsPerReply) + 1))
              sort
              (Range Nothing Nothing)
          .| ( C.awaitForever $ \v@(Entity strikeId _)-> do
              C.toProducer $ C.zipSources
                (repeatC v)
                (streamEntities
                  (( BlockTimeStrikeGuessStrike ==. strikeId ):(maybe [] (buildFilter . unFilterRequest . mapFilter) mfilter ))
                  BlockTimeStrikeGuessId
                  (PageSize ((fromPositive recordsPerReply) + 1))
                  sort
                  (Range Nothing Nothing)
                )
             )
          .| (C.drop (fromNatural page * fromPositive recordsPerReply) >> C.awaitForever C.yield) -- navigate to page
          .| ( C.awaitForever $ \(Entity _ strike, Entity _ guess) -> do
              mperson <- lift $ get (blockTimeStrikeGuessPerson guess)
              case ( mperson) of
                (Nothing ) -> return ()
                ( Just person) -> do
                  C.yield $ BlockTimeStrikeGuessPublic
                            { person = personUuid person
                            , strike = strike
                            , creationTime = blockTimeStrikeGuessCreationTime guess
                            , guess = blockTimeStrikeGuessGuess guess
                            }
             )
          .| C.take (fromPositive recordsPerReply + 1) -- we take +1 to understand if there is a next page available
        return (count, res)
      case mret of
          Nothing -> do
            throwError err500 {errBody = "something went wrong, check logs for details"}
          Just (count, guessesTail) -> do
            let newPage =
                  if List.length guessesTail > fromPositive recordsPerReply
                  then Just (fromIntegral (fromNatural page + 1))
                  else Nothing
                results = List.take (fromPositive recordsPerReply) guessesTail
            return $ PagingResult
              { pagingResultNextPage = newPage
              , pagingResultCount = fromIntegral count
              , pagingResultResults = results
              }
  where
    repeatC v = C.yield v >> repeatC v
    sort = maybe Descend (sortOrder . unFilterRequest . id1 . mapFilter) mfilter
      where
        id1 :: FilterRequest BlockTimeStrike BlockTimeStrikeGuessResultPublicFilter -> FilterRequest BlockTimeStrike BlockTimeStrikeGuessResultPublicFilter
        id1 = id -- helping typechecker

-- | returns list BlockTimeStrike records
getBlockTimeStrikeGuessPage
  :: Maybe (Natural Int)
  -> Maybe (FilterRequest BlockTimeStrikeGuess BlockTimeStrikeGuessResultPublicFilter)
  -> AppM (PagingResult BlockTimeStrikeGuessPublic)
getBlockTimeStrikeGuessPage mpage mfilter = profile "getBlockTimeStrikeGuessPage" $ do
  let
      filter = (maybe [] (buildFilter . unFilterRequest . mapFilter ) mfilter)
  mret <- pagingResult mpage filter sort BlockTimeStrikeGuessId
    $ ( C.awaitForever $ \(Entity _ guess)-> do
          mstrike <- lift $ get (blockTimeStrikeGuessStrike guess)
          mperson <- lift $ get (blockTimeStrikeGuessPerson guess)
          case (mstrike, mperson) of
            (Nothing, _ ) -> return ()
            ( _, Nothing) -> return ()
            (Just strike, Just person) -> do
              C.yield $ BlockTimeStrikeGuessPublic
                { person = personUuid person
                , strike = strike
                , creationTime = blockTimeStrikeGuessCreationTime guess
                , guess = blockTimeStrikeGuessGuess guess
                }
      )
  case mret of
    Nothing -> do
      throwError err500 {errBody = "something went wrong"}
    Just ret -> return ret
  where
    sort = maybe Descend (sortOrder . unFilterRequest . id1 . mapFilter) mfilter
      where
        id1 :: FilterRequest BlockTimeStrike BlockTimeStrikeGuessResultPublicFilter -> FilterRequest BlockTimeStrike BlockTimeStrikeGuessResultPublicFilter
        id1 = id -- helping typechecker
