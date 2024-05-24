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
  , getBlockTimeStrikesGuessesPage
  , getBlockTimeStrikeGuessesPage
  , getBlockTimeStrikeGuess
  , getBlockTimeStrikeGuessPerson
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
import           Data.OpEnergy.Account.API.V1.UUID
import           Data.OpEnergy.Account.API.V1.BlockTimeStrikeFilterClass
import           OpEnergy.Account.Server.V1.Config (Config(..))
import           OpEnergy.Account.Server.V1.Class ( AppT, AppM, State(..), runLogging, profile, withDBTransaction, withDBNOTransactionRO)
import qualified OpEnergy.BlockTimeStrike.Server.V1.Class as BlockTime
import           OpEnergy.Account.Server.V1.AccountService (mgetPersonByAccountToken)

mgetBlockTimeStrikeFuture :: (MonadIO m, MonadMonitor m) => BlockHeight-> Natural Int-> AppT m (Maybe (Entity BlockTimeStrike))
mgetBlockTimeStrikeFuture blockHeight strikeMediantime = profile "mgetBlockTimeStrikeFuture" $ do
  mret <- withDBTransaction "" $ do
    selectFirst [ BlockTimeStrikeBlock ==. blockHeight
                , BlockTimeStrikeStrikeMediantime ==. fromIntegral strikeMediantime
                ] []
  case mret of
    Just some -> return some
    _ -> return Nothing

-- | O(ln accounts).
-- Tries to create future block time strike. Requires authenticated user and blockheight should be in the future
createBlockTimeStrikeFutureGuess :: AccountToken-> BlockHeight-> Natural Int-> SlowFast-> AppM BlockTimeStrikeGuessPublic
createBlockTimeStrikeFutureGuess token blockHeight strikeMediantime guess = profile "createBlockTimeStrikeFutureGuess" $ do
  configBlockTimeStrikeGuessMinimumBlockAheadCurrentTip <- asks (configBlockTimeStrikeGuessMinimumBlockAheadCurrentTip . config)
  latestConfirmedBlockV <- asks (BlockTime.latestConfirmedBlock . blockTimeState)
  mlatestConfirmedBlock <- liftIO $ TVar.readTVarIO latestConfirmedBlockV
  case mlatestConfirmedBlock of
    Nothing -> do
      let err = "ERROR: createBlockTimeStrikeFutureGuess: there is no current tip yet"
      runLogging $ $(logError) err
      throwError err500 {errBody = BS.fromStrict (Text.encodeUtf8 err)}
    Just tip
        | blockHeaderMediantime tip > fromIntegral strikeMediantime -> do
        let err = "ERROR: createBlockTimeStrikeFutureGuess: strikeMediantime is in the past, which is not expected"
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
        Just (Entity personKey person) -> do
          mstrike <- mgetBlockTimeStrikeFuture blockHeight strikeMediantime
          case mstrike of
            Nothing-> do
              let err = "ERROR: createBlockTimeStrikeFutureGuess: future strike was not able to authenticate itself"
              runLogging $ $(logError) err
              throwError err400 {errBody = BS.fromStrict (Text.encodeUtf8 err)}
            Just (Entity strikeKey strike) -> do
              mret <- createBlockTimeStrikeFutureGuess personKey strikeKey guess
              case mret of
                Nothing -> throwError err500 {errBody = "something went wrong"}
                Just v -> return $ BlockTimeStrikeGuessPublic
                  { person = personUuid person
                  , strike = strike
                  , creationTime = blockTimeStrikeGuessCreationTime v
                  , guess = guess
                  }
  where
    createBlockTimeStrikeFutureGuess :: (MonadMonitor m, MonadIO m) => Key Person-> Key BlockTimeStrike-> SlowFast-> AppT m (Maybe BlockTimeStrikeGuess)
    createBlockTimeStrikeFutureGuess personKey strikeKey guess = profile "createBlockTimeStrikeFutureGuess" $ do
      nowUTC <- liftIO getCurrentTime
      let now = utcTimeToPOSIXSeconds nowUTC
      withDBTransaction "" $ do
        let value = BlockTimeStrikeGuess
              { blockTimeStrikeGuessGuess = guess
              , blockTimeStrikeGuessCreationTime = now
              , blockTimeStrikeGuessPerson = personKey
              , blockTimeStrikeGuessStrike = strikeKey
              }
        _ <- insert value
        return value

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
          $ filters recordsPerReply confirmedBlock
          .| ( do
                 let
                     loop (acc::Int) = do
                       mv <- C.await
                       case mv of
                         Nothing -> return acc
                         Just _ -> do
                           loop (acc + 1)
                 loop 0
             )
        res <- C.runConduit
          $ filters recordsPerReply confirmedBlock
          .| (C.drop (fromNatural page * fromPositive recordsPerReply) >> C.awaitForever C.yield) -- navigate to page
          .| ( C.awaitForever $ \((Entity _ strike, Entity _ guess), Entity _ person) -> do
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
    filters recordsPerReply confirmedBlock =
      streamEntities
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
      .| ( C.awaitForever $ \v@(_, Entity _ guess )-> do
          C.toProducer $ C.zipSources
            (repeatC v)
            ( streamEntities
              (( PersonId ==. blockTimeStrikeGuessPerson guess ):(maybe [] (buildFilter . unFilterRequest . mapFilter) mfilter ))
              PersonId
              (PageSize ((fromPositive recordsPerReply) + 1))
              sort
              (Range Nothing Nothing)
            )
         )

-- | returns list BlockTimeStrikeGuess records
getBlockTimeStrikesGuessesPage
  :: Maybe (Natural Int)
  -> Maybe (FilterRequest BlockTimeStrikeGuess BlockTimeStrikeGuessResultPublicFilter)
  -> AppM (PagingResult BlockTimeStrikeGuessPublic)
getBlockTimeStrikesGuessesPage mpage mfilter = profile "getBlockTimeStrikesGuessesPage" $ do
  recordsPerReply <- asks (configRecordsPerReply . config)
  latestConfirmedBlockV <- asks (BlockTime.latestConfirmedBlock . blockTimeState)
  mlatestConfirmedBlock <- liftIO $ TVar.readTVarIO latestConfirmedBlockV
  averageBlockDiscoverSecs <- asks (configAverageBlockDiscoverSecs . config)
  minimumBlocksAhead <- asks (configBlockTimeStrikeMinimumBlockAheadCurrentTip . config)
  case mlatestConfirmedBlock of
    Nothing -> do
      let msg = "getBlockTimeStrikesGuessesPage: no confirmed block found yet"
      runLogging $  $(logError) msg
      throwError err500 { errBody = BS.fromStrict $ Text.encodeUtf8 msg}
    Just confirmedBlock -> do
      let finalFilter =
            case maybe Nothing (blockTimeStrikeGuessResultPublicFilterClass . fst . unFilterRequest) mfilter of
              Nothing -> filter
              Just BlockTimeStrikeFilterClassGuessable ->
                ( (BlockTimeStrikeBlock >=. (blockHeaderHeight confirmedBlock + naturalFromPositive minimumBlocksAhead))
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
      mret <- withDBNOTransactionRO "" $ do
        count <- C.runConduit
          $ filters finalFilter recordsPerReply
          .| ( do
                let
                    loop (acc::Int) = do
                      mv <- C.await
                      case mv of
                        Nothing -> return acc
                        Just _-> do
                          loop (acc + 1)
                loop 0
            )
        res <- C.runConduit
          $ filters finalFilter recordsPerReply
          .| (C.drop (fromNatural page * fromPositive recordsPerReply) >> C.awaitForever C.yield) -- navigate to page
          .| ( C.awaitForever $ \((Entity _ strike, Entity _ guess), Entity _ person) -> do
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
    page = maybe 0 id mpage
    filter = maybe [] (buildFilter . unFilterRequest . mapFilter) mfilter
    repeatC v = C.yield v >> repeatC v
    sort = maybe Descend (sortOrder . unFilterRequest . id1 . mapFilter) mfilter
      where
        id1 :: FilterRequest BlockTimeStrike BlockTimeStrikeGuessResultPublicFilter -> FilterRequest BlockTimeStrike BlockTimeStrikeGuessResultPublicFilter
        id1 = id -- helping typechecker
    filters finalFilter recordsPerReply =
      streamEntities
          finalFilter
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
      .| ( C.awaitForever $ \v@(_, Entity _ guess )-> do
          C.toProducer $ C.zipSources
            (repeatC v)
            ( streamEntities
              (( PersonId ==. blockTimeStrikeGuessPerson guess ):(maybe [] (buildFilter . unFilterRequest . mapFilter) mfilter ))
              PersonId
              (PageSize ((fromPositive recordsPerReply) + 1))
              sort
              (Range Nothing Nothing)
            )
         )

-- | returns list BlockTimeStrikeGuesses records
getBlockTimeStrikeGuessesPage
  :: BlockHeight
  -> Natural Int
  -> Maybe (Natural Int)
  -> Maybe (FilterRequest BlockTimeStrikeGuess BlockTimeStrikeGuessResultPublicFilter)
  -> AppM (PagingResult BlockTimeStrikeGuessPublic)
getBlockTimeStrikeGuessesPage blockHeight strikeMediantime mpage mfilter = profile "getBlockTimeStrikesGuessesPage" $ do
  recordsPerReply <- asks (configRecordsPerReply . config)
  mret <- withDBNOTransactionRO "" $ do
    count <- C.runConduit
      $ filters recordsPerReply
      .| ( do
             let
                 loop (acc::Int) = do
                   mv <- C.await
                   case mv of
                     Nothing -> return acc
                     Just _-> do
                       loop (acc + 1)
             loop 0
         )
    res <- C.runConduit
      $ filters recordsPerReply
      .| (C.drop (fromNatural page * fromPositive recordsPerReply) >> C.awaitForever C.yield) -- navigate to page
      .| ( C.awaitForever $ \((Entity _ strike, Entity _ guess), Entity _ person) -> do
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
    page = maybe 0 id mpage
    repeatC v = C.yield v >> repeatC v
    sort = maybe Descend (sortOrder . unFilterRequest . id1 . mapFilter) mfilter
      where
        id1 :: FilterRequest BlockTimeStrike BlockTimeStrikeGuessResultPublicFilter -> FilterRequest BlockTimeStrike BlockTimeStrikeGuessResultPublicFilter
        id1 = id -- helping typechecker
    filters recordsPerReply =
      streamEntities
          [ BlockTimeStrikeBlock ==. blockHeight, BlockTimeStrikeStrikeMediantime ==. fromIntegral strikeMediantime ]
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
      .| ( C.awaitForever $ \v@(_, Entity _ guess )-> do
          C.toProducer $ C.zipSources
            (repeatC v)
            ( streamEntities
              (( PersonId ==. blockTimeStrikeGuessPerson guess ):(maybe [] (buildFilter . unFilterRequest . mapFilter) mfilter ))
              PersonId
              (PageSize ((fromPositive recordsPerReply) + 1))
              sort
              (Range Nothing Nothing)
            )
         )

-- | returns BlockTimeStrikeGuessPublic by strike and person, taken from account token
getBlockTimeStrikeGuess
  :: AccountToken
  -> BlockHeight
  -> Natural Int
  -> AppM BlockTimeStrikeGuessPublic
getBlockTimeStrikeGuess token blockHeight strikeMediantime = profile "getBlockTimeStrikeGuess" $ do
  mperson <- mgetPersonByAccountToken token
  case mperson of
    Nothing-> do
      let err = "ERROR: getBlockTimeStrikeGuess: person was not able to authenticate itself"
      runLogging $ $(logError) err
      throwError err400 {errBody = BS.fromStrict (Text.encodeUtf8 err)}
    Just personE -> do
      mstrike <- actualGetStrikeGuess personE blockHeight (fromIntegral strikeMediantime)
      case mstrike of
        Nothing-> do
          let err = "ERROR: getBlockTimeStrikeGuess: something went wrong, see logs for details"
          runLogging $ $(logError) err
          throwError err500 {errBody = BS.fromStrict (Text.encodeUtf8 err)}
        Just Nothing -> do
          let err = "ERROR: getBlockTimeStrikeGuess: no strike or guess found"
          runLogging $ $(logError) err
          throwError err400 {errBody = BS.fromStrict (Text.encodeUtf8 err)}
        Just (Just ret)-> return ret

  where
    repeatC v = C.yield v >> repeatC v
    actualGetStrikeGuess (Entity personKey person) blockHeight strikeMediantime = do
      recordsPerReply <- asks (configRecordsPerReply . config)
      withDBNOTransactionRO "" $ do
        C.runConduit
          $ streamEntities
            [BlockTimeStrikeBlock ==. blockHeight, BlockTimeStrikeStrikeMediantime ==. strikeMediantime]
            BlockTimeStrikeId
            (PageSize ((fromPositive recordsPerReply) + 1))
            Descend
            (Range Nothing Nothing)
          .| ( C.awaitForever $ \v@(Entity strikeId _)-> do
              C.toProducer $ C.zipSources
                (repeatC v)
                (streamEntities
                  [ BlockTimeStrikeGuessStrike ==. strikeId, BlockTimeStrikeGuessPerson ==. personKey ]
                  BlockTimeStrikeGuessId
                  (PageSize ((fromPositive recordsPerReply) + 1))
                  Descend
                  (Range Nothing Nothing)
                )
             )
          .| ( let loop = do
                     mv <- C.await
                     case mv of
                       Nothing -> return Nothing
                       Just (Entity _ strike, Entity _ guess) -> return $ Just $ BlockTimeStrikeGuessPublic
                         { person = personUuid person
                         , strike = strike
                         , creationTime = blockTimeStrikeGuessCreationTime guess
                         , guess = blockTimeStrikeGuessGuess guess
                         }
               in loop
             )

getBlockTimeStrikeGuessPerson
  :: UUID Person
  -> BlockHeight
  -> Natural Int
  -> AppM BlockTimeStrikeGuessPublic
getBlockTimeStrikeGuessPerson uuid blockHeight strikeMediantime = profile "getBlockTimeStrikeGuessPerson" $ do
  mperson <- mgetPersonByUUID uuid
  case mperson of
    Nothing -> do
      let err = "ERROR: getBlockTimeStrikeGuessPerson: something went wrong, check logs"
      runLogging $ $(logError) err
      throwError err500 {errBody = BS.fromStrict (Text.encodeUtf8 err)}
    Just Nothing-> do
      let err = "ERROR: getBlockTimeStrikeGuessPerson: person was not able to authenticate itself"
      runLogging $ $(logError) err
      throwError err400 {errBody = BS.fromStrict (Text.encodeUtf8 err)}
    Just (Just personE) -> do
      mstrike <- actualGetStrikeGuess personE blockHeight (fromIntegral strikeMediantime)
      case mstrike of
        Nothing-> do
          let err = "ERROR: getBlockTimeStrikeGuess: something went wrong, see logs for details"
          runLogging $ $(logError) err
          throwError err500 {errBody = BS.fromStrict (Text.encodeUtf8 err)}
        Just Nothing -> do
          let err = "ERROR: getBlockTimeStrikeGuess: no strike or guess found"
          runLogging $ $(logError) err
          throwError err400 {errBody = BS.fromStrict (Text.encodeUtf8 err)}
        Just (Just ret)-> return ret

  where
    repeatC v = C.yield v >> repeatC v
    mgetPersonByUUID uuid = do
      withDBNOTransactionRO "" $ do
        selectFirst [ PersonUuid ==. uuid ][]
    actualGetStrikeGuess (Entity personKey person) blockHeight strikeMediantime = do
      recordsPerReply <- asks (configRecordsPerReply . config)
      withDBNOTransactionRO "" $ do
        C.runConduit
          $ streamEntities
            [BlockTimeStrikeBlock ==. blockHeight, BlockTimeStrikeStrikeMediantime ==. strikeMediantime]
            BlockTimeStrikeId
            (PageSize ((fromPositive recordsPerReply) + 1))
            Descend
            (Range Nothing Nothing)
          .| ( C.awaitForever $ \v@(Entity strikeId _)-> do
              C.toProducer $ C.zipSources
                (repeatC v)
                (streamEntities
                  [ BlockTimeStrikeGuessStrike ==. strikeId, BlockTimeStrikeGuessPerson ==. personKey ]
                  BlockTimeStrikeGuessId
                  (PageSize ((fromPositive recordsPerReply) + 1))
                  Descend
                  (Range Nothing Nothing)
                )
             )
          .| ( let loop = do
                     mv <- C.await
                     case mv of
                       Nothing -> return Nothing
                       Just (Entity _ strike, Entity _ guess) -> return $ Just $ BlockTimeStrikeGuessPublic
                         { person = personUuid person
                         , strike = strike
                         , creationTime = blockTimeStrikeGuessCreationTime guess
                         , guess = blockTimeStrikeGuessGuess guess
                         }
               in loop
             )
