{-- | This module implements BlockTime strike service.
 -}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
module OpEnergy.BlockTimeStrike.Server.V1.BlockTimeStrikeGuessService
  ( createBlockTimeStrikeFutureGuess
  , createBlockTimeStrikeFutureGuessHandler
  , getBlockTimeStrikeGuessResultsPage
  , getBlockTimeStrikesGuessesPageHandler
  , getBlockTimeStrikesGuessesPage
  , getBlockTimeStrikeGuessesPage
  , getBlockTimeStrikeGuessesPageHandler
  , getBlockTimeStrikeGuess
  , getBlockTimeStrikeGuessPerson
  ) where

import           Servant (err400, err500)
import           Control.Monad.Trans.Reader (asks, ReaderT(..))
import           Control.Monad.Logger( logError, NoLoggingT)
import           Data.Time.Clock( getCurrentTime)
import           Data.Time.Clock.POSIX(POSIXTime, utcTimeToPOSIXSeconds)
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TVar as TVar
import           Control.Monad(void, when)
import qualified Data.List as List
import           Data.Text(Text)
import           Control.Monad.Trans.Resource( ResourceT)
import           Control.Monad.Trans.Except( runExceptT, ExceptT(..), throwE)
import           Data.Maybe(fromMaybe)

import           Data.Conduit ((.|), ConduitT)
import qualified Data.Conduit as C
import qualified Data.Conduit.List as C
import qualified Data.Conduit.Extra.Zip as C
import           Control.Monad.Trans
import           Database.Persist.Postgresql
import           Database.Persist.Pagination
import           Prometheus(MonadMonitor)


import qualified Data.OpEnergy.Account.API.V1.Account as API
import           Data.OpEnergy.API.V1.Positive( naturalFromPositive
                                              , fromPositive
                                              , Positive
                                              )
import           Data.OpEnergy.API.V1.Block
import           Data.OpEnergy.API.V1.Natural
import qualified Data.OpEnergy.Account.API.V1.BlockTimeStrike as API
import qualified Data.OpEnergy.Account.API.V1.BlockTimeStrikeGuess as API
import qualified Data.OpEnergy.Account.API.V1.SlowFast as API
import qualified Data.OpEnergy.Account.API.V1.PagingResult as API
import qualified Data.OpEnergy.Account.API.V1.FilterRequest as API
import           Data.OpEnergy.Account.API.V1.UUID
import           Data.OpEnergy.Account.API.V1.BlockTimeStrikeFilterClass
import           Data.OpEnergy.API.V1.Error (throwJSON)

import           OpEnergy.ExceptMaybe(exceptTMaybeT)
import           OpEnergy.Error( eitherThrowJSON, runExceptPrefixT)
import           OpEnergy.PagingResult( pagingResult)
import           OpEnergy.Account.Server.V1.Config (Config(..))
import           OpEnergy.Account.Server.V1.Class ( AppT, AppM, State(..), runLogging, profile, withDBTransaction)
import           OpEnergy.Account.Server.V1.AccountService (mgetPersonByAccountToken)
import           OpEnergy.Account.Server.V1.Person
import qualified OpEnergy.BlockTimeStrike.Server.V1.Class as BlockTime
import qualified OpEnergy.BlockTimeStrike.Server.V1.BlockTimeStrikeFilter as BlockTimeStrikeFilter
import           OpEnergy.BlockTimeStrike.Server.V1.BlockTimeStrike
import           OpEnergy.BlockTimeStrike.Server.V1.BlockTimeStrikeGuess
import qualified OpEnergy.BlockTimeStrike.Server.V1.SlowFast as SlowFast

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
createBlockTimeStrikeFutureGuessHandler
  :: API.AccountToken
  -> BlockHeight
  -> Natural Int
  -> API.SlowFast
  -> AppM API.BlockTimeStrikeGuess
createBlockTimeStrikeFutureGuessHandler
    token blockHeight strikeMediantime guess =
    let name = "BlockTimeStrikeGuessService.createBlockTimeStrikeFutureGuessHandler"
    in profile name $
  eitherThrowJSON
    (\reason-> do
      let msg = "getBlockTimeStrikesGuessesPage: " <> reason
      runLogging $ $(logError) msg
      return (err500, msg)
    )
    $ createBlockTimeStrikeFutureGuess token blockHeight strikeMediantime guess

createBlockTimeStrikeFutureGuess
  :: API.AccountToken
  -> BlockHeight
  -> Natural Int
  -> API.SlowFast
  -> AppM (Either Text API.BlockTimeStrikeGuess)
createBlockTimeStrikeFutureGuess token blockHeight strikeMediantime guess =
    let name = "BlockTimeStrikeGuessService.createBlockTimeStrikeFutureGuess"
    in profile name $ runExceptPrefixT name $ do
  configBlockTimeStrikeGuessMinimumBlockAheadCurrentTip <- lift
    $ asks (configBlockTimeStrikeGuessMinimumBlockAheadCurrentTip . config)
  latestConfirmedBlockV <- lift
    $ asks (BlockTime.latestConfirmedBlock . blockTimeState)
  tip <- exceptTMaybeT "there is no current tip yet"
    $ liftIO $ TVar.readTVarIO latestConfirmedBlockV
  when (blockHeaderMediantime tip > fromIntegral strikeMediantime)
    $ throwE "strikeMediantime is in the past, which is not expected"
  when ( blockHeaderHeight tip
       + naturalFromPositive configBlockTimeStrikeGuessMinimumBlockAheadCurrentTip
       > blockHeight)
    $ throwE "block height for new block time strike should be in the future \
             \+ minimum configBlockTimeStrikeFutureGuessMinimumBlockAheadCurrentTip"
  Entity personKey person <- exceptTMaybeT
    "person was not able to authenticate itself"
    $ mgetPersonByAccountToken token
  Entity strikeKey strike <- exceptTMaybeT
    "future strike was not able to authenticate itself"
    $ mgetBlockTimeStrikeFuture blockHeight strikeMediantime
  v <- exceptTMaybeT
    "something went wrong"
    $ createBlockTimeStrikeFutureGuess personKey strikeKey guess
  return $ API.BlockTimeStrikeGuess
    { API.person = apiModelUUIDPerson $ personUuid person
    , API.strike = apiModelBlockTimeStrike strike Nothing
    , API.creationTime = blockTimeStrikeGuessCreationTime v
    , API.guess = guess
    }
  where
    createBlockTimeStrikeFutureGuess
      :: (MonadMonitor m, MonadIO m)
      => Key Person
      -> Key BlockTimeStrike
      -> API.SlowFast
      -> AppT m (Maybe BlockTimeStrikeGuess)
    createBlockTimeStrikeFutureGuess personKey strikeKey guess = profile "createBlockTimeStrikeFutureGuess" $ do
      nowUTC <- liftIO getCurrentTime
      let now = utcTimeToPOSIXSeconds nowUTC
      withDBTransaction "" $ do
        let value = BlockTimeStrikeGuess
              { blockTimeStrikeGuessIsFast = SlowFast.modelApi guess
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
  -> Maybe (API.FilterRequest API.BlockTimeStrikeGuess API.BlockTimeStrikeGuessFilter)
  -> AppM (API.PagingResult API.BlockTimeStrikeGuess)
getBlockTimeStrikeGuessResultsPage mpage mfilterAPI = profile "getBlockTimeStrikeGuessResultsPage" $ do
  mconfirmedBlockV <- asks ( BlockTime.latestConfirmedBlock . blockTimeState)
  mconfirmedBlock <- liftIO $ TVar.readTVarIO mconfirmedBlockV
  case mconfirmedBlock of
    Nothing -> do
      let err = "ERROR: getBlockTimeStrikeGuessResultsPage: no confirmed block found yet"
      runLogging $ $(logError) err
      throwJSON err500 err
    Just confirmedBlock -> do
      let
          page = fromMaybe 0 mpage
      recordsPerReply <- asks (configRecordsPerReply . config)
      let
          linesPerPage = maybe
            recordsPerReply
            ( fromMaybe recordsPerReply
            . API.blockTimeStrikeGuessFilterLinesPerPage
            . fst
            . API.unFilterRequest
            )
            mfilter
      mret <- withDBTransaction "" $ C.runConduit
        $ filters linesPerPage confirmedBlock
        .| (C.drop (fromNatural page * fromPositive linesPerPage) >> C.awaitForever C.yield) -- navigate to page
        .| C.map renderBlockTimeStrikeGuessResult
        .| C.take (fromPositive linesPerPage + 1) -- we take +1 to understand if there is a next page available
      case mret of
          Nothing -> do
            throwJSON err500 ("something went wrong, check logs for details"::Text)
          Just guessesTail -> do
            let newPage =
                  if List.length guessesTail > fromPositive linesPerPage
                  then Just (fromIntegral (fromNatural page + 1))
                  else Nothing
                results = List.take (fromPositive linesPerPage) guessesTail
            return $ API.PagingResult
              { API.pagingResultNextPage = newPage
              , API.pagingResultResults = results
              }
  where
    mfilter = fmap coerceFilterRequestBlockTimeStrikeGuess mfilterAPI
    sort = maybe Descend (API.sortOrder . API.unFilterRequest . id1 . API.mapFilter) mfilter
      where
        id1
          :: API.FilterRequest BlockTimeStrike API.BlockTimeStrikeGuessFilter
          -> API.FilterRequest BlockTimeStrike API.BlockTimeStrikeGuessFilter
        id1 = id -- helping typechecker
    filters recordsPerReply confirmedBlock =
      fetchConfirmedStrikes recordsPerReply confirmedBlock
      .| C.zipConcatMapMC (fetchBlockTimeStrikeGuessByStrike
           recordsPerReply sort mfilter
         )
      .| C.zipConcatMapMC (fetchGuessPersonByBlockTimeStrikeGuess recordsPerReply sort mfilter)
      .| C.mapM maybeFetchObservedStrikeByStrikeGuessAndPersonAndFlatten
    -- unique
    fetchConfirmedStrikes
      :: Positive Int
      -> BlockHeader
      -> ConduitT
         ()
         (Entity BlockTimeStrike)
         (ReaderT SqlBackend (NoLoggingT (ResourceT IO)))
         ()
    fetchConfirmedStrikes recordsPerReply confirmedBlock = streamEntities
      ((BlockTimeStrikeBlock <=. blockHeaderHeight confirmedBlock)
      : maybe [] (API.buildFilter . API.unFilterRequest . API.mapFilter) mfilter
      )
      BlockTimeStrikeId
      (PageSize (fromPositive recordsPerReply + 1))
      sort
      (Range Nothing Nothing)



-- | returns list BlockTimeStrikeGuess records
getBlockTimeStrikesGuessesPageHandler
  :: Maybe (Natural Int)
  -> Maybe (API.FilterRequest API.BlockTimeStrikeGuess API.BlockTimeStrikeGuessFilter)
  -> AppM (API.PagingResult API.BlockTimeStrikeGuess)
getBlockTimeStrikesGuessesPageHandler mpage mfilterAPI =
    let name = "getBlockTimeStrikesGuessesPageHandler"
    in profile name $ do
  eitherThrowJSON
    (\reason-> do
      let msg = "getBlockTimeStrikesGuessesPage: " <> reason
      runLogging $ $(logError) msg
      return (err500, msg)
    )
    $ getBlockTimeStrikesGuessesPage mpage mfilterAPI

-- | returns list BlockTimeStrikeGuess records
getBlockTimeStrikesGuessesPage
  :: Maybe (Natural Int)
  -> Maybe (API.FilterRequest API.BlockTimeStrikeGuess API.BlockTimeStrikeGuessFilter)
  -> AppM (Either Text (API.PagingResult API.BlockTimeStrikeGuess))
getBlockTimeStrikesGuessesPage mpage mfilterAPI =
    let name = "getBlockTimeStrikesGuessesPage"
    in profile name $ runExceptPrefixT name $ do
  latestUnconfirmedBlockHeightV <- lift
    $ asks (BlockTime.latestUnconfirmedBlockHeight . blockTimeState)
  configBlockTimeStrikeGuessMinimumBlockAheadCurrentTip <- lift
    $ asks (configBlockTimeStrikeGuessMinimumBlockAheadCurrentTip . config)
  latestConfirmedBlockV <- lift
    $ asks (BlockTime.latestConfirmedBlock . blockTimeState)
  recordsPerReply <- lift
    $ asks (configRecordsPerReply . config)
  let
      linesPerPage = maybe
        recordsPerReply
        ( fromMaybe recordsPerReply
        . API.blockTimeStrikeGuessFilterLinesPerPage
        . fst
        . API.unFilterRequest
        )
        mfilter
  (latestUnconfirmedBlockHeight, latestConfirmedBlock) <- ExceptT
    $ liftIO $ STM.atomically $ runExceptT $ (,)
      <$> (exceptTMaybeT "latest unconfirmed block hasn't been received yet"
          $ TVar.readTVar latestUnconfirmedBlockHeightV
          )
      <*> ( exceptTMaybeT "latest confirmed block hasn't been received yet"
          $ TVar.readTVar latestConfirmedBlockV
          )
  let
      finalStrikesFilter =
        BlockTimeStrikeFilter.buildFilterByClass
          (maybe Nothing (API.blockTimeStrikeGuessFilterClass . fst . API.unFilterRequest) mfilter)
          latestUnconfirmedBlockHeight
          latestConfirmedBlock
          configBlockTimeStrikeGuessMinimumBlockAheadCurrentTip
        ++ strikeFilter
  exceptTMaybeT "db query failed"
    $ pagingResult
      mpage
      linesPerPage
      guessFilter
      sort
      BlockTimeStrikeGuessId
      $ streamGuessStrikeObservedResultAndOwner finalStrikesFilter linesPerPage
  where
    mfilter = fmap coerceFilterRequestBlockTimeStrikeGuess mfilterAPI
    strikeFilter :: [Filter BlockTimeStrike]
    strikeFilter = maybe [] (API.buildFilter . API.unFilterRequest . API.mapFilter) mfilter
    guessFilter :: [ Filter BlockTimeStrikeGuess]
    guessFilter = maybe [] (API.buildFilter . API.unFilterRequest . API.mapFilter) mfilter
    sort = maybe Descend (API.sortOrder . API.unFilterRequest . id1 . API.mapFilter) mfilter
      where
        id1
          :: API.FilterRequest BlockTimeStrike API.BlockTimeStrikeGuessFilter
          -> API.FilterRequest BlockTimeStrike API.BlockTimeStrikeGuessFilter
        id1 = id -- helping typechecker

    streamGuessStrikeObservedResultAndOwner
      :: [Filter BlockTimeStrike]
      -> Positive Int
      -> ConduitT
           (Entity BlockTimeStrikeGuess)
           API.BlockTimeStrikeGuess
           (ReaderT SqlBackend (NoLoggingT (ResourceT IO)))
           ()
    streamGuessStrikeObservedResultAndOwner finalFilter linesPerPage
      =  C.zipConcatMapMC fetchPerGuessBlockTimeStrikeByGuess
      .| C.mapM possiblyFetchObservedResultAndZipWithGuessAndStrike
      .| C.zipConcatMapMC fetchPerGuessOwnerAndZipObservedResultGuessAndStrike
      .| C.map reorgTupleToFitRenderBlockTimeGuessResult
      .| C.map renderBlockTimeStrikeGuessResult
      where
        -- unique
        fetchPerGuessBlockTimeStrikeByGuess
          :: Entity BlockTimeStrikeGuess
          -> ConduitT
             ()
             (Entity BlockTimeStrike)
             (ReaderT SqlBackend (NoLoggingT (ResourceT IO)))
             ()
        fetchPerGuessBlockTimeStrikeByGuess (Entity _ guess) = streamEntities
          ((BlockTimeStrikeId ==. blockTimeStrikeGuessStrike guess)
          :finalFilter
          )
          BlockTimeStrikeId
          (PageSize (fromPositive linesPerPage + 1))
          sort
          (Range Nothing Nothing)
        -- unique
        possiblyFetchObservedResultAndZipWithGuessAndStrike
          :: (Entity BlockTimeStrikeGuess, Entity BlockTimeStrike)
          -> (ReaderT SqlBackend (NoLoggingT (ResourceT IO)))
             (Entity BlockTimeStrikeGuess
             , Entity BlockTimeStrike
             , Maybe (Entity BlockTimeStrikeObserved)
             )
        possiblyFetchObservedResultAndZipWithGuessAndStrike
            (guess, strikeE@(Entity strikeId _)) = do
          case maybe
              Nothing
              (API.blockTimeStrikeGuessFilterClass
              . fst
              . API.unFilterRequest
              )
              mfilter of
            Nothing -> do
              mObserved <- selectFirst
                [ BlockTimeStrikeObservedStrike ==. strikeId ]
                []
              return (guess, strikeE, mObserved) -- don't care about existence of observed data
            Just BlockTimeStrikeFilterClassGuessable-> do
              return (guess, strikeE, Nothing) -- strike has not been observed and finalFilter should ensure, that it is in the future with proper guess threshold
            Just BlockTimeStrikeFilterClassOutcomeUnknown-> do
              return (guess, strikeE, Nothing) -- hasn't been observed
            Just BlockTimeStrikeFilterClassOutcomeKnown-> do
              mObserved <- selectFirst
                [ BlockTimeStrikeObservedStrike ==. strikeId ]
                []
              return (guess, strikeE, mObserved) -- had been observed
        -- TODO: unique?
        fetchPerGuessOwnerAndZipObservedResultGuessAndStrike
          :: ( Entity BlockTimeStrikeGuess
             , Entity BlockTimeStrike
             , Maybe (Entity BlockTimeStrikeObserved)
             )
          -> ConduitT
             ()
             (Entity Person)
             (ReaderT SqlBackend (NoLoggingT (ResourceT IO)))
             ()
        fetchPerGuessOwnerAndZipObservedResultGuessAndStrike
            (Entity _ guess, _, _) =
          streamEntities
            (( PersonId ==. blockTimeStrikeGuessPerson guess )
            : maybe [] (API.buildFilter . API.unFilterRequest . API.mapFilter) mfilter
            )
            PersonId
            (PageSize (fromPositive linesPerPage + 1))
            sort
            (Range Nothing Nothing)
        reorgTupleToFitRenderBlockTimeGuessResult
          :: ( ( Entity BlockTimeStrikeGuess
               , Entity BlockTimeStrike
               , Maybe (Entity BlockTimeStrikeObserved)
               )
             , Entity Person
             )
          -> ( Entity BlockTimeStrike
             , Entity BlockTimeStrikeGuess
             , Entity Person
             , Maybe (Entity BlockTimeStrikeObserved)
             )
        reorgTupleToFitRenderBlockTimeGuessResult
            ((guessE, strikeE, mObserved), personE) =
          (strikeE, guessE, personE, mObserved)


-- | returns list BlockTimeStrikeGuesses records
getBlockTimeStrikeGuessesPageHandler
  :: BlockHeight
  -> Natural Int
  -> Maybe (Natural Int)
  -> Maybe (API.FilterRequest API.BlockTimeStrikeGuess API.BlockTimeStrikeGuessFilter)
  -> AppM (API.PagingResult API.BlockTimeStrikeGuess)
getBlockTimeStrikeGuessesPageHandler
  strikeBlockHeight strikeMediantime mpage mfilterAPI =
    let name = "V1.BlockTimeStrikeGuessService.getBlockTimeStrikeGuessesPageHandler"
    in profile name $
  eitherThrowJSON
    (\reason-> do
      let msg = name <> ": " <> reason
      runLogging $ $(logError) msg
      return (err500, msg)
    )
    $ getBlockTimeStrikeGuessesPage strikeBlockHeight strikeMediantime mpage
      mfilterAPI

-- | returns list BlockTimeStrikeGuesses records
getBlockTimeStrikeGuessesPage
  :: BlockHeight
  -> Natural Int
  -> Maybe (Natural Int)
  -> Maybe (API.FilterRequest API.BlockTimeStrikeGuess API.BlockTimeStrikeGuessFilter)
  -> AppM (Either Text (API.PagingResult API.BlockTimeStrikeGuess))
getBlockTimeStrikeGuessesPage blockHeight strikeMediantime mpage mfilterAPI =
    let name = "getBlockTimeStrikeGuessesPage"
    in profile name $ runExceptPrefixT name $ do
  recordsPerReply <- lift $ asks (configRecordsPerReply . config)
  let
      linesPerPage = maybe
                       recordsPerReply
                       ( fromMaybe recordsPerReply
                       . API.blockTimeStrikeGuessFilterLinesPerPage
                       . fst
                       . API.unFilterRequest
                       )
                       mfilter
  guessesTail <- exceptTMaybeT "something went wrong, check logs for details"
    $ withDBTransaction "" $ do
      C.runConduit
        $ filters linesPerPage
        .| (C.drop (fromNatural page * fromPositive linesPerPage) >> C.awaitForever C.yield) -- navigate to page
        .| C.map renderBlockTimeStrikeGuessResult
        .| C.take (fromPositive linesPerPage + 1) -- we take +1 to understand if there is a next page available
  let newPage =
        if List.length guessesTail > fromPositive linesPerPage
        then Just (fromIntegral (fromNatural page + 1))
        else Nothing
      results = List.take (fromPositive linesPerPage) guessesTail
  return $ API.PagingResult
    { API.pagingResultNextPage = newPage
    , API.pagingResultResults = results
    }
  where
    page = fromMaybe 0 mpage
    mfilter = fmap coerceFilterRequestBlockTimeStrikeGuess mfilterAPI
    sort = maybe Descend (API.sortOrder . API.unFilterRequest . id1 . API.mapFilter) mfilter
      where
        id1
          :: API.FilterRequest BlockTimeStrike API.BlockTimeStrikeGuessFilter
          -> API.FilterRequest BlockTimeStrike API.BlockTimeStrikeGuessFilter
        id1 = id -- helping typechecker
    filters recordsPerReply =
      fetchBlockTimeStrikeByHeightAndMediantime recordsPerReply sort
        blockHeight (fromIntegral strikeMediantime)
      .| C.zipConcatMapMC (fetchBlockTimeStrikeGuessByStrike
           recordsPerReply sort mfilter
         )
      .| C.zipConcatMapMC (fetchGuessPersonByBlockTimeStrikeGuess
           recordsPerReply sort mfilter
         )
      .| C.mapM maybeFetchObservedStrikeByStrikeGuessAndPersonAndFlatten

-- | returns BlockTimeStrikeGuess by strike and person, taken from account token
getBlockTimeStrikeGuess
  :: API.AccountToken
  -> BlockHeight
  -> Natural Int
  -> AppM API.BlockTimeStrikeGuess
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
    actualGetStrikeGuess (Entity personKey person) blockHeight strikeMediantime = do
      recordsPerReply <- asks (configRecordsPerReply . config)
      withDBTransaction "" $ do
        C.runConduit
          $ fetchBlockTimeStrikeByHeightAndMediantime
              recordsPerReply
              Descend
              blockHeight
              strikeMediantime
          .| C.zipConcatMapMC (fetchBlockTimeStrikeGuessByStrikeAndPerson
               recordsPerReply
               personKey
             )
          .| C.mapM maybeFetchObservedStrike
          .| C.map (renderBlockTimeStrikeGuessResultByPerson person)
          .| C.head
    maybeFetchObservedStrike
      :: (Entity BlockTimeStrike, Entity BlockTimeStrikeGuess)
      -> ReaderT SqlBackend (NoLoggingT (ResourceT IO))
         ( Entity BlockTimeStrike
         , Entity BlockTimeStrikeGuess
         , Maybe (Entity BlockTimeStrikeObserved)
         )
    maybeFetchObservedStrike ( strikeE@(Entity strikeId _), guessE) = do
      mObserved <- selectFirst
        [ BlockTimeStrikeObservedStrike ==. strikeId ]
        []
      return (strikeE, guessE, mObserved)

getBlockTimeStrikeGuessPerson
  :: UUID API.Person
  -> BlockHeight
  -> Natural Int
  -> AppM API.BlockTimeStrikeGuess
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
    mgetPersonByUUID uuid = do
      withDBTransaction "" $ do
        selectFirst [ PersonUuid ==. modelApiUUIDPerson uuid ][]
    actualGetStrikeGuess (Entity personKey person) blockHeight strikeMediantime = do
      recordsPerReply <- asks (configRecordsPerReply . config)
      withDBTransaction "" $ do
        C.runConduit
          $ fetchBlockTimeStrikeByHeightAndMediantime recordsPerReply Descend blockHeight strikeMediantime
          .| C.zipConcatMapMC (fetchBlockTimeStrikeGuessByStrikeAndPerson
               recordsPerReply personKey
             )
          .| C.mapM maybeFetchBlockTimeStrikeObserved
          .| C.map (renderBlockTimeStrikeGuessResultByPerson person)
          .| C.head
    maybeFetchBlockTimeStrikeObserved
      :: (Entity BlockTimeStrike, Entity BlockTimeStrikeGuess)
      -> ReaderT
         SqlBackend
         (NoLoggingT (ResourceT IO))
         (Entity BlockTimeStrike
         , Entity BlockTimeStrikeGuess
         , Maybe (Entity BlockTimeStrikeObserved)
         )
    maybeFetchBlockTimeStrikeObserved (strikeE@(Entity strikeId _), guessE) = do
      mObserved <- selectFirst
        [ BlockTimeStrikeObservedStrike ==. strikeId]
        []
      return (strikeE, guessE, mObserved)

fetchBlockTimeStrikeByHeightAndMediantime
  :: Positive Int
  -> SortOrder
  -> BlockHeight
  -> POSIXTime
  -> ConduitT
     ()
     (Entity BlockTimeStrike)
     (ReaderT SqlBackend (NoLoggingT (ResourceT IO)))
     ()
fetchBlockTimeStrikeByHeightAndMediantime
    recordsPerReply sort blockHeight strikeMediantime = streamEntities
  [ BlockTimeStrikeBlock ==. blockHeight
  , BlockTimeStrikeStrikeMediantime ==. strikeMediantime
  ]
  BlockTimeStrikeId
  (PageSize (fromPositive recordsPerReply + 1))
  sort
  (Range Nothing Nothing)


renderBlockTimeStrikeGuessResultByPerson
  :: Person
  -> ( Entity BlockTimeStrike
     , Entity BlockTimeStrikeGuess
     , Maybe (Entity BlockTimeStrikeObserved)
     )
  -> API.BlockTimeStrikeGuess
renderBlockTimeStrikeGuessResultByPerson person
    (Entity _ strike, Entity _ guess, mObserved) =
  API.BlockTimeStrikeGuess
    { API.person = apiModelUUIDPerson $ personUuid person
    , API.strike = API.BlockTimeStrike
      { API.blockTimeStrikeObservedResult = fmap
          (\(Entity _ result)->
            SlowFast.apiModel $ blockTimeStrikeObservedIsFast result
          )
          mObserved
      , API.blockTimeStrikeObservedBlockMediantime = fmap
          (\(Entity _ result)->
            blockTimeStrikeObservedJudgementBlockMediantime result
          )
          mObserved
      , API.blockTimeStrikeObservedBlockHash = fmap
          (\(Entity _ result)->
            blockTimeStrikeObservedJudgementBlockHash result
          )
          mObserved
      , API.blockTimeStrikeObservedBlockHeight = fmap
          (\(Entity _ result)->
            blockTimeStrikeObservedJudgementBlockHeight result
          )
          mObserved
      , API.blockTimeStrikeBlock = blockTimeStrikeBlock strike
      , API.blockTimeStrikeStrikeMediantime =
        blockTimeStrikeStrikeMediantime strike
      , API.blockTimeStrikeCreationTime =
        blockTimeStrikeCreationTime strike
      }
    , API.creationTime = blockTimeStrikeGuessCreationTime guess
    , API.guess = SlowFast.apiModel $ blockTimeStrikeGuessIsFast guess
    }

fetchBlockTimeStrikeGuessByStrike
  :: Positive Int
  -> SortOrder
  -> Maybe (API.FilterRequest BlockTimeStrikeGuess API.BlockTimeStrikeGuessFilter)
  -> Entity BlockTimeStrike
  -> ConduitT
     ()
     (Entity BlockTimeStrikeGuess)
     (ReaderT SqlBackend (NoLoggingT (ResourceT IO)))
     ()
fetchBlockTimeStrikeGuessByStrike recordsPerReply sort mfilter (Entity strikeId _) = do
  streamEntities
    ( ( BlockTimeStrikeGuessStrike ==. strikeId )
    : maybe [] (API.buildFilter . API.unFilterRequest . API.mapFilter) mfilter
    )
    BlockTimeStrikeGuessId
    (PageSize (fromPositive recordsPerReply + 1))
    sort
    (Range Nothing Nothing)

fetchGuessPersonByBlockTimeStrikeGuess
  :: Positive Int
  -> SortOrder
  -> Maybe (API.FilterRequest BlockTimeStrikeGuess API.BlockTimeStrikeGuessFilter)
  -> ( Entity BlockTimeStrike, Entity BlockTimeStrikeGuess)
  -> ConduitT
     ()
     (Entity Person)
     (ReaderT SqlBackend (NoLoggingT (ResourceT IO)))
     ()
fetchGuessPersonByBlockTimeStrikeGuess
    recordsPerReply sort mfilter (_, Entity _ guess ) = do
  streamEntities
    ( ( PersonId ==. blockTimeStrikeGuessPerson guess )
    : maybe [] (API.buildFilter . API.unFilterRequest . API.mapFilter) mfilter
    )
    PersonId
    (PageSize (fromPositive recordsPerReply + 1))
    sort
    (Range Nothing Nothing)

maybeFetchObservedStrikeByStrikeGuessAndPersonAndFlatten
  :: ( ( Entity BlockTimeStrike
       , Entity BlockTimeStrikeGuess
       )
     , Entity Person
     )
  -> ReaderT SqlBackend (NoLoggingT (ResourceT IO))
       ( Entity BlockTimeStrike
       , Entity BlockTimeStrikeGuess
       , Entity Person
       , Maybe (Entity BlockTimeStrikeObserved)
       )
maybeFetchObservedStrikeByStrikeGuessAndPersonAndFlatten
    ((strikeE@(Entity strikeId _), guessE), personE) = do
  mObserved <- selectFirst
    [ BlockTimeStrikeObservedStrike ==. strikeId]
    []
  return (strikeE, guessE, personE, mObserved)

renderBlockTimeStrikeGuessResult
  :: ( Entity BlockTimeStrike
     , Entity BlockTimeStrikeGuess
     , Entity Person
     , Maybe (Entity BlockTimeStrikeObserved)
     )
  -> API.BlockTimeStrikeGuess
renderBlockTimeStrikeGuessResult
    (Entity _ strike, Entity _ guess, Entity _ person, mObserved) =
  API.BlockTimeStrikeGuess
    { person = apiModelUUIDPerson $ personUuid person
    , strike = API.BlockTimeStrike
      { blockTimeStrikeObservedResult = fmap
        (\(Entity _ result) -> SlowFast.apiModel $ blockTimeStrikeObservedIsFast result)
        mObserved
      , blockTimeStrikeObservedBlockMediantime = fmap
        (\(Entity _ result) ->
          blockTimeStrikeObservedJudgementBlockMediantime result
        )
        mObserved
      , blockTimeStrikeObservedBlockHash = fmap
        (\(Entity _ result) ->
          blockTimeStrikeObservedJudgementBlockHash result
        )
        mObserved
      , blockTimeStrikeObservedBlockHeight = fmap
        (\(Entity _ result) ->
          blockTimeStrikeObservedJudgementBlockHeight result
        )
        mObserved
      , blockTimeStrikeBlock = blockTimeStrikeBlock strike
      , blockTimeStrikeStrikeMediantime =
        blockTimeStrikeStrikeMediantime strike
      , blockTimeStrikeCreationTime =
        blockTimeStrikeCreationTime strike
      }
    , creationTime = blockTimeStrikeGuessCreationTime guess
    , guess = SlowFast.apiModel $ blockTimeStrikeGuessIsFast guess
    }

fetchBlockTimeStrikeGuessByStrikeAndPerson
  :: Positive Int
  -> PersonId
  -> Entity BlockTimeStrike
  -> ConduitT
     ()
     (Entity BlockTimeStrikeGuess)
     (ReaderT SqlBackend (NoLoggingT (ResourceT IO)))
     ()
fetchBlockTimeStrikeGuessByStrikeAndPerson
    recordsPerReply personKey (Entity strikeId _) = do
  streamEntities
    [ BlockTimeStrikeGuessStrike ==. strikeId
    , BlockTimeStrikeGuessPerson ==. personKey
    ]
    BlockTimeStrikeGuessId
    (PageSize (fromPositive recordsPerReply + 1))
    Descend
    (Range Nothing Nothing)

