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

import           Servant (err400, err500)
import           Control.Monad.Trans.Reader (asks)
import           Control.Monad.Logger( logError)
import           Data.Time.Clock(getCurrentTime)
import           Data.Time.Clock.POSIX(utcTimeToPOSIXSeconds)
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TVar as TVar
import           Control.Monad(void)
import qualified Data.List as List
import           Data.Text(Text)
import           Control.Monad.Trans.Except( runExceptT, ExceptT(..))

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
import           Data.OpEnergy.Account.API.V1.BlockTimeStrikePublic
import           Data.OpEnergy.Account.API.V1.BlockTimeStrikeGuess
import           Data.OpEnergy.Account.API.V1.PagingResult
import           Data.OpEnergy.Account.API.V1.FilterRequest
import           Data.OpEnergy.Account.API.V1.UUID
import           Data.OpEnergy.Account.API.V1.BlockTimeStrikeFilterClass
import           Data.OpEnergy.API.V1.Error (throwJSON)
import           OpEnergy.Account.Server.V1.Config (Config(..))
import           OpEnergy.Account.Server.V1.Class ( AppT, AppM, State(..), runLogging, profile, withDBTransaction, withDBNOTransactionROUnsafe)
import qualified OpEnergy.BlockTimeStrike.Server.V1.Class as BlockTime
import           OpEnergy.Account.Server.V1.AccountService (mgetPersonByAccountToken)
import           OpEnergy.ExceptMaybe(exceptTMaybeT)
import           OpEnergy.Error( eitherThrowJSON)
import           OpEnergy.PagingResult( pagingResult)
import qualified OpEnergy.BlockTimeStrike.Server.V1.BlockTimeStrikeFilter as BlockTimeStrikeFilter

mgetBlockTimeStrikeFuture
  :: (MonadIO m, MonadMonitor m)
  => BlockHeight
  -> Natural Int
  -> AppT m (Maybe (Entity BlockTimeStrike))
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
createBlockTimeStrikeFutureGuess
  :: AccountToken
  -> BlockHeight
  -> Natural Int
  -> SlowFast
  -> AppM BlockTimeStrikeGuessPublic
createBlockTimeStrikeFutureGuess token blockHeight strikeMediantime guess = profile "createBlockTimeStrikeFutureGuess" $ do
  configBlockTimeStrikeGuessMinimumBlockAheadCurrentTip <- asks (configBlockTimeStrikeGuessMinimumBlockAheadCurrentTip . config)
  latestConfirmedBlockV <- asks (BlockTime.latestConfirmedBlock . blockTimeState)
  mlatestConfirmedBlock <- liftIO $ TVar.readTVarIO latestConfirmedBlockV
  case mlatestConfirmedBlock of
    Nothing -> do
      let err = "ERROR: createBlockTimeStrikeFutureGuess: there is no current tip yet"
      runLogging $ $(logError) err
      throwJSON err500 err
    Just tip
        | blockHeaderMediantime tip > fromIntegral strikeMediantime -> do
        let err = "ERROR: createBlockTimeStrikeFutureGuess: strikeMediantime is in the past, which is not expected"
        runLogging $ $(logError) err
        throwJSON err400 err
    Just tip
      | blockHeaderHeight tip + naturalFromPositive configBlockTimeStrikeGuessMinimumBlockAheadCurrentTip > blockHeight -> do
        let err = "ERROR: createBlockTimeStrikeFutureGuess: block height for new block time strike should be in the future + minimum configBlockTimeStrikeFutureGuessMinimumBlockAheadCurrentTip"
        runLogging $ $(logError) err
        throwJSON err400 err
    _ -> do
      mperson <- mgetPersonByAccountToken token
      case mperson of
        Nothing-> do
          let err = "ERROR: createBlockTimeStrikeFutureGuess: person was not able to authenticate itself"
          runLogging $ $(logError) err
          throwJSON err400 err
        Just (Entity personKey person) -> do
          mstrike <- mgetBlockTimeStrikeFuture blockHeight strikeMediantime
          case mstrike of
            Nothing-> do
              let err = "ERROR: createBlockTimeStrikeFutureGuess: future strike was not able to authenticate itself"
              runLogging $ $(logError) err
              throwJSON err400 err
            Just (Entity strikeKey strike) -> do
              mret <- createBlockTimeStrikeFutureGuess personKey strikeKey guess
              case mret of
                Nothing -> throwJSON err500 ("something went wrong"::Text)
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
              { blockTimeStrikeGuessIsFast = guess
              , blockTimeStrikeGuessCreationTime = now
              , blockTimeStrikeGuessPerson = personKey
              , blockTimeStrikeGuessStrike = strikeKey
              }
        _ <- insert value
        mguessesCount <- selectFirst [ CalculatedBlockTimeStrikeGuessesCountStrike ==. strikeKey ][]
        case mguessesCount of
          Just (Entity guessesCountId _) -> -- there is a record of precalculated counts, update it
            update guessesCountId
              [ CalculatedBlockTimeStrikeGuessesCountGuessesCount +=. verifyNatural 1
              ]
          Nothing -> do -- no record exist yet, recalculate to be idempotent TODO: maybe it is a bad place to recalculate and we need some kind of scheduled task for this
            guessesCount <- count [ BlockTimeStrikeGuessStrike ==. strikeKey ]
            void $! insert $! CalculatedBlockTimeStrikeGuessesCount
              { calculatedBlockTimeStrikeGuessesCountGuessesCount = verifyNatural guessesCount
              , calculatedBlockTimeStrikeGuessesCountStrike = strikeKey
              }
        return value

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
      throwJSON err500 err
    Just confirmedBlock -> do
      let
          page = maybe 0 id mpage
      recordsPerReply <- asks (configRecordsPerReply . config)
      let
          linesPerPage = maybe recordsPerReply (maybe recordsPerReply id . blockTimeStrikeGuessResultPublicFilterLinesPerPage . fst . unFilterRequest ) mfilter
      mret <- withDBNOTransactionROUnsafe "" $ do
        res <- C.runConduit
          $ filters linesPerPage confirmedBlock
          .| (C.drop (fromNatural page * fromPositive linesPerPage) >> C.awaitForever C.yield) -- navigate to page
          .| ( C.awaitForever $ \(Entity _ strike, Entity _ guess, Entity _ person, mObserved) -> do
               C.yield $ BlockTimeStrikeGuessResultPublic
                         { person = personUuid person
                         , strike = BlockTimeStrikePublic
                           { blockTimeStrikePublicObservedResult = maybe
                             Nothing
                             (\(Entity _ result) -> Just (blockTimeStrikeObservedIsFast result))
                             mObserved
                           , blockTimeStrikePublicObservedBlockMediantime = maybe
                             Nothing
                             (\(Entity _ result) -> Just (blockTimeStrikeObservedJudgementBlockMediantime result))
                             mObserved
                           , blockTimeStrikePublicObservedBlockHash = maybe
                             Nothing
                             (\(Entity _ result) -> Just (blockTimeStrikeObservedJudgementBlockHash result))
                             mObserved
                           , blockTimeStrikePublicObservedBlockHeight = maybe
                             Nothing
                             (\(Entity _ result) -> Just (blockTimeStrikeObservedJudgementBlockHeight result))
                             mObserved
                           , blockTimeStrikePublicBlock = blockTimeStrikeBlock strike
                           , blockTimeStrikePublicStrikeMediantime = blockTimeStrikeStrikeMediantime strike
                           , blockTimeStrikePublicCreationTime = blockTimeStrikeCreationTime strike
                           }
                         , creationTime = blockTimeStrikeGuessCreationTime guess
                         , guess = blockTimeStrikeGuessIsFast guess
                         }
             )
          .| C.take (fromPositive linesPerPage + 1) -- we take +1 to understand if there is a next page available
        return (res)
      case mret of
          Nothing -> do
            throwJSON err500 ("something went wrong, check logs for details"::Text)
          Just (guessesTail) -> do
            let newPage =
                  if List.length guessesTail > fromPositive linesPerPage
                  then Just (fromIntegral (fromNatural page + 1))
                  else Nothing
                results = List.take (fromPositive linesPerPage) guessesTail
            return $ PagingResult
              { pagingResultNextPage = newPage
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
      .| ( C.awaitForever $ \((strikeE@(Entity strikeId _), guessE), personE) -> do
           mObserved <- lift $ selectFirst [ BlockTimeStrikeObservedStrike ==. strikeId][]
           C.yield (strikeE, guessE, personE, mObserved)
         )

-- | returns list BlockTimeStrikeGuess records
getBlockTimeStrikesGuessesPage
  :: Maybe (Natural Int)
  -> Maybe (FilterRequest BlockTimeStrikeGuess BlockTimeStrikeGuessResultPublicFilter)
  -> AppM (PagingResult BlockTimeStrikeGuessResultPublic)
getBlockTimeStrikesGuessesPage mpage mfilter = profile "getBlockTimeStrikesGuessesPage" $ do
  latestUnconfirmedBlockHeightV <- asks (BlockTime.latestUnconfirmedBlockHeight . blockTimeState)
  configBlockTimeStrikeGuessMinimumBlockAheadCurrentTip <- asks (configBlockTimeStrikeGuessMinimumBlockAheadCurrentTip . config)
  latestConfirmedBlockV <- asks (BlockTime.latestConfirmedBlock . blockTimeState)
  recordsPerReply <- asks (configRecordsPerReply . config)
  let
      linesPerPage = maybe recordsPerReply (maybe recordsPerReply id . blockTimeStrikeGuessResultPublicFilterLinesPerPage . fst . unFilterRequest ) mfilter
  eitherThrowJSON
    (\reason-> do
      let msg = "getBlockTimeStrikesGuessesPage: " <> reason
      runLogging $ $(logError) msg
      return (err500, msg)
    )
    $ runExceptT $ do
      (latestUnconfirmedBlockHeight, latestConfirmedBlock) <- ExceptT
        $ liftIO $ STM.atomically $ runExceptT $ (,)
          <$> (exceptTMaybeT "latest unconfirmed block hasn't been received yet"
              $ TVar.readTVar latestUnconfirmedBlockHeightV
              )
          <*> ( exceptTMaybeT "latest confirmed block hasn't been received yet"
              $ TVar.readTVar latestConfirmedBlockV
              )
      let
          finalStrikesFilter = BlockTimeStrikeFilter.buildFilter
            strikeFilter
            (maybe Nothing (blockTimeStrikeGuessResultPublicFilterClass . fst . unFilterRequest) mfilter)
            latestUnconfirmedBlockHeight
            latestConfirmedBlock
            configBlockTimeStrikeGuessMinimumBlockAheadCurrentTip
      exceptTMaybeT ("db query failed")
        $ pagingResult
          mpage
          linesPerPage
          guessFilter
          sort
          BlockTimeStrikeGuessId
          $ streamGuessStrikeObservedResultAndOwner finalStrikesFilter linesPerPage
  where
    strikeFilter = maybe [] (buildFilter . unFilterRequest . mapFilter) mfilter
    guessFilter = maybe [] (buildFilter . unFilterRequest . mapFilter) mfilter
    repeatC v = C.yield v >> repeatC v
    sort = maybe Descend (sortOrder . unFilterRequest . id1 . mapFilter) mfilter
      where
        id1 :: FilterRequest BlockTimeStrike BlockTimeStrikeGuessResultPublicFilter -> FilterRequest BlockTimeStrike BlockTimeStrikeGuessResultPublicFilter
        id1 = id -- helping typechecker

    streamGuessStrikeObservedResultAndOwner finalFilter linesPerPage
      = fetchPerGuessBlockTimeStrikeAndZipThem
      .| possiblyFetchObservedResultAndZipWithGuessAndStrike
      .| fetchPerGuessOwnerAndZipObservedResultGuessAndStrike
      .| buildBlockTimeStrikeGuessResultPublic
      where
        fetchPerGuessBlockTimeStrikeAndZipThem =
          C.awaitForever $ \v@(Entity _ guess)-> do
            C.toProducer $ C.zipSources
              (repeatC v)
              (streamEntities
                ((BlockTimeStrikeId ==. blockTimeStrikeGuessStrike guess):finalFilter)
                BlockTimeStrikeId
                (PageSize ((fromPositive linesPerPage) + 1))
                sort
                (Range Nothing Nothing)
              )
        possiblyFetchObservedResultAndZipWithGuessAndStrike =
          C.awaitForever $ \(guess, strikeE@(Entity strikeId _)) -> do
            case maybe Nothing (blockTimeStrikeGuessResultPublicFilterClass . fst . unFilterRequest) mfilter of
              Nothing -> do
                mObserved <- lift $ selectFirst [ BlockTimeStrikeObservedStrike ==. strikeId ][]
                C.yield (guess, strikeE, mObserved) -- don't care about existence of observed data
              Just BlockTimeStrikeFilterClassGuessable-> do
                C.yield (guess, strikeE, Nothing) -- strike has not been observed and finalFilter should ensure, that it is in the future with proper guess threshold
              Just BlockTimeStrikeFilterClassOutcomeUnknown-> do
                C.yield (guess, strikeE, Nothing) -- hasn't been observed
              Just BlockTimeStrikeFilterClassOutcomeKnown-> do
                mObserved <- lift $ selectFirst [ BlockTimeStrikeObservedStrike ==. strikeId ][]
                C.yield (guess, strikeE, mObserved) -- had been observed
        fetchPerGuessOwnerAndZipObservedResultGuessAndStrike =
          C.awaitForever $ \v@( Entity _ guess, _, _ )-> do
            C.toProducer $ C.zipSources
              (repeatC v)
              ( streamEntities
                (( PersonId ==. blockTimeStrikeGuessPerson guess ):(maybe [] (buildFilter . unFilterRequest . mapFilter) mfilter ))
                PersonId
                (PageSize ((fromPositive linesPerPage) + 1))
                sort
                (Range Nothing Nothing)
              )
        buildBlockTimeStrikeGuessResultPublic =
          C.awaitForever $ \((Entity _ guess, Entity _ strike, mObserved), Entity _ person) -> do
            C.yield $ BlockTimeStrikeGuessResultPublic
              { person = personUuid person
              , strike = BlockTimeStrikePublic
                { blockTimeStrikePublicObservedResult = maybe
                  Nothing
                  (\(Entity _ result) -> Just (blockTimeStrikeObservedIsFast result) )
                  mObserved
                , blockTimeStrikePublicObservedBlockMediantime = maybe
                  Nothing
                  (\(Entity _ result) -> Just (blockTimeStrikeObservedJudgementBlockMediantime result) )
                  mObserved
                , blockTimeStrikePublicObservedBlockHash = maybe
                  Nothing
                  (\(Entity _ result) -> Just (blockTimeStrikeObservedJudgementBlockHash result) )
                  mObserved
                , blockTimeStrikePublicObservedBlockHeight = maybe
                  Nothing
                  (\(Entity _ result) -> Just (blockTimeStrikeObservedJudgementBlockHeight result) )
                  mObserved
                , blockTimeStrikePublicBlock = blockTimeStrikeBlock strike
                , blockTimeStrikePublicStrikeMediantime = blockTimeStrikeStrikeMediantime strike
                , blockTimeStrikePublicCreationTime = blockTimeStrikeCreationTime strike
                }
              , creationTime = blockTimeStrikeGuessCreationTime guess
              , guess = blockTimeStrikeGuessIsFast guess
              }


-- | returns list BlockTimeStrikeGuesses records
getBlockTimeStrikeGuessesPage
  :: BlockHeight
  -> Natural Int
  -> Maybe (Natural Int)
  -> Maybe (FilterRequest BlockTimeStrikeGuess BlockTimeStrikeGuessResultPublicFilter)
  -> AppM (PagingResult BlockTimeStrikeGuessResultPublic)
getBlockTimeStrikeGuessesPage blockHeight strikeMediantime mpage mfilter = profile "getBlockTimeStrikeGuessesPage" $ do
  recordsPerReply <- asks (configRecordsPerReply . config)
  let
      linesPerPage = maybe recordsPerReply (maybe recordsPerReply id . blockTimeStrikeGuessResultPublicFilterLinesPerPage . fst . unFilterRequest ) mfilter
  mret <- withDBNOTransactionROUnsafe "" $ do
    res <- C.runConduit
      $ filters linesPerPage
      .| (C.drop (fromNatural page * fromPositive linesPerPage) >> C.awaitForever C.yield) -- navigate to page
      .| ( C.awaitForever $ \(Entity _ strike, Entity _ guess, Entity _ person, mObserved) -> do
           C.yield $ BlockTimeStrikeGuessResultPublic
                     { person = personUuid person
                     , strike = BlockTimeStrikePublic
                       { blockTimeStrikePublicObservedResult = maybe
                         Nothing
                         (\(Entity _ result)-> Just (blockTimeStrikeObservedIsFast result))
                         mObserved
                       , blockTimeStrikePublicObservedBlockMediantime = maybe
                         Nothing
                         (\(Entity _ result)-> Just (blockTimeStrikeObservedJudgementBlockMediantime result))
                         mObserved
                       , blockTimeStrikePublicObservedBlockHash = maybe
                         Nothing
                         (\(Entity _ result)-> Just (blockTimeStrikeObservedJudgementBlockHash result))
                         mObserved
                       , blockTimeStrikePublicObservedBlockHeight = maybe
                         Nothing
                         (\(Entity _ result)-> Just (blockTimeStrikeObservedJudgementBlockHeight result))
                         mObserved
                       , blockTimeStrikePublicBlock = blockTimeStrikeBlock strike
                       , blockTimeStrikePublicStrikeMediantime = blockTimeStrikeStrikeMediantime strike
                       , blockTimeStrikePublicCreationTime = blockTimeStrikeCreationTime strike
                       }
                     , creationTime = blockTimeStrikeGuessCreationTime guess
                     , guess = blockTimeStrikeGuessIsFast guess
                     }
         )
      .| C.take (fromPositive linesPerPage + 1) -- we take +1 to understand if there is a next page available
    return (res)
  case mret of
      Nothing -> do
        throwJSON err500 ("something went wrong, check logs for details"::Text)
      Just (guessesTail) -> do
        let newPage =
              if List.length guessesTail > fromPositive linesPerPage
              then Just (fromIntegral (fromNatural page + 1))
              else Nothing
            results = List.take (fromPositive linesPerPage) guessesTail
        return $ PagingResult
          { pagingResultNextPage = newPage
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
      .| ( C.awaitForever $ \((strikeE@(Entity strikeId _), guessE), personE)-> do
           mObserved <- lift $ selectFirst [ BlockTimeStrikeObservedStrike ==. strikeId ][]
           C.yield (strikeE, guessE, personE, mObserved)
         )

-- | returns BlockTimeStrikeGuessPublic by strike and person, taken from account token
getBlockTimeStrikeGuess
  :: AccountToken
  -> BlockHeight
  -> Natural Int
  -> AppM BlockTimeStrikeGuessResultPublic
getBlockTimeStrikeGuess token blockHeight strikeMediantime = profile "getBlockTimeStrikeGuess" $ do
  mperson <- mgetPersonByAccountToken token
  case mperson of
    Nothing-> do
      let err = "ERROR: getBlockTimeStrikeGuess: person was not able to authenticate itself"
      runLogging $ $(logError) err
      throwJSON err400 err
    Just personE -> do
      mstrike <- actualGetStrikeGuess personE blockHeight (fromIntegral strikeMediantime)
      case mstrike of
        Nothing-> do
          let err = "ERROR: getBlockTimeStrikeGuess: something went wrong, see logs for details"
          runLogging $ $(logError) err
          throwJSON err500 err
        Just Nothing -> do
          let err = "ERROR: getBlockTimeStrikeGuess: no strike or guess found"
          runLogging $ $(logError) err
          throwJSON err400 err
        Just (Just ret)-> return ret

  where
    repeatC v = C.yield v >> repeatC v
    actualGetStrikeGuess (Entity personKey person) blockHeight strikeMediantime = do
      recordsPerReply <- asks (configRecordsPerReply . config)
      withDBNOTransactionROUnsafe "" $ do
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
          .| ( C.awaitForever $ \( strikeE@(Entity strikeId _), guessE)-> do
               mObserved <- lift $ selectFirst [ BlockTimeStrikeObservedStrike ==. strikeId ][]
               C.yield (strikeE, guessE, mObserved)
             )
          .| ( let loop = do
                     mv <- C.await
                     case mv of
                       Nothing -> return Nothing
                       Just (Entity _ strike, Entity _ guess, mObserved) -> return $ Just $ BlockTimeStrikeGuessResultPublic
                         { person = personUuid person
                         , strike = BlockTimeStrikePublic
                           { blockTimeStrikePublicObservedResult = maybe
                             Nothing
                             (\(Entity _ result) -> Just (blockTimeStrikeObservedIsFast result))
                             mObserved
                           , blockTimeStrikePublicObservedBlockMediantime = maybe
                             Nothing
                             (\(Entity _ result) -> Just (blockTimeStrikeObservedJudgementBlockMediantime result))
                             mObserved
                           , blockTimeStrikePublicObservedBlockHash = maybe
                             Nothing
                             (\(Entity _ result) -> Just (blockTimeStrikeObservedJudgementBlockHash result))
                             mObserved
                           , blockTimeStrikePublicObservedBlockHeight = maybe
                             Nothing
                             (\(Entity _ result) -> Just (blockTimeStrikeObservedJudgementBlockHeight result))
                             mObserved
                           , blockTimeStrikePublicBlock = blockTimeStrikeBlock strike
                           , blockTimeStrikePublicStrikeMediantime = blockTimeStrikeStrikeMediantime strike
                           , blockTimeStrikePublicCreationTime = blockTimeStrikeCreationTime strike
                           }
                         , creationTime = blockTimeStrikeGuessCreationTime guess
                         , guess = blockTimeStrikeGuessIsFast guess
                         }
               in loop
             )

getBlockTimeStrikeGuessPerson
  :: UUID Person
  -> BlockHeight
  -> Natural Int
  -> AppM BlockTimeStrikeGuessResultPublic
getBlockTimeStrikeGuessPerson uuid blockHeight strikeMediantime = profile "getBlockTimeStrikeGuessPerson" $ do
  mperson <- mgetPersonByUUID uuid
  case mperson of
    Nothing -> do
      let err = "ERROR: getBlockTimeStrikeGuessPerson: something went wrong, check logs"
      runLogging $ $(logError) err
      throwJSON err500 err
    Just Nothing-> do
      let err = "ERROR: getBlockTimeStrikeGuessPerson: person was not able to authenticate itself"
      runLogging $ $(logError) err
      throwJSON err400 err
    Just (Just personE) -> do
      mstrike <- actualGetStrikeGuess personE blockHeight (fromIntegral strikeMediantime)
      case mstrike of
        Nothing-> do
          let err = "ERROR: getBlockTimeStrikeGuess: something went wrong, see logs for details"
          runLogging $ $(logError) err
          throwJSON err500 err
        Just Nothing -> do
          let err = "ERROR: getBlockTimeStrikeGuess: no strike or guess found"
          runLogging $ $(logError) err
          throwJSON err400 err
        Just (Just ret)-> return ret

  where
    repeatC v = C.yield v >> repeatC v
    mgetPersonByUUID uuid = do
      withDBNOTransactionROUnsafe "" $ do
        selectFirst [ PersonUuid ==. uuid ][]
    actualGetStrikeGuess (Entity personKey person) blockHeight strikeMediantime = do
      recordsPerReply <- asks (configRecordsPerReply . config)
      withDBNOTransactionROUnsafe "" $ do
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
          .| ( C.awaitForever $ \(strikeE@(Entity strikeId _), guessE) -> do
               mObserved <- lift $ selectFirst [ BlockTimeStrikeObservedStrike ==. strikeId][]
               C.yield (strikeE, guessE, mObserved)
             )
          .| ( let loop = do
                     mv <- C.await
                     case mv of
                       Nothing -> return Nothing
                       Just (Entity _ strike, Entity _ guess, mObserved) -> return $ Just $ BlockTimeStrikeGuessResultPublic
                         { person = personUuid person
                         , strike = BlockTimeStrikePublic
                           { blockTimeStrikePublicObservedResult =
                             maybe
                               Nothing
                               (\(Entity _ result)-> Just (blockTimeStrikeObservedIsFast result))
                               mObserved
                           , blockTimeStrikePublicObservedBlockMediantime =
                             maybe
                               Nothing
                               (\(Entity _ result)-> Just (blockTimeStrikeObservedJudgementBlockMediantime result))
                               mObserved
                           , blockTimeStrikePublicObservedBlockHash =
                             maybe
                               Nothing
                               (\(Entity _ result)-> Just (blockTimeStrikeObservedJudgementBlockHash result))
                               mObserved
                           , blockTimeStrikePublicObservedBlockHeight =
                             maybe
                               Nothing
                               (\(Entity _ result)-> Just (blockTimeStrikeObservedJudgementBlockHeight result))
                               mObserved
                           , blockTimeStrikePublicBlock = blockTimeStrikeBlock strike
                           , blockTimeStrikePublicStrikeMediantime = blockTimeStrikeStrikeMediantime strike
                           , blockTimeStrikePublicCreationTime = blockTimeStrikeCreationTime strike
                           }
                         , creationTime = blockTimeStrikeGuessCreationTime guess
                         , guess = blockTimeStrikeGuessIsFast guess
                         }
               in loop
             )

