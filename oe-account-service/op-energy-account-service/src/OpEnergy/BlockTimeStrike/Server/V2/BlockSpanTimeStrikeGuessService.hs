{-- | This module implements BlockSpanTime strike service.
 -}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
module OpEnergy.BlockTimeStrike.Server.V2.BlockSpanTimeStrikeGuessService
  ( createBlockSpanTimeStrikeFutureGuess
  , getBlockSpanTimeStrikeGuessResultsPage
  , getBlockSpanTimeStrikesGuessesPage
  , getBlockSpanTimeStrikeGuessesPage
  , getBlockSpanTimeStrikeGuess
  , getBlockSpanTimeStrikeGuessPerson
  ) where

import           Control.Monad.Trans.Reader (asks, ReaderT(..))
import           Control.Monad.Logger( NoLoggingT)
import           Data.Time.Clock( getCurrentTime)
import           Data.Time.Clock.POSIX(POSIXTime, utcTimeToPOSIXSeconds)
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TVar as TVar
import           Control.Monad(when, void)
import qualified Data.List as List
import           Control.Monad.Trans.Resource( ResourceT)
import           Control.Monad.Trans.Except
                   ( runExceptT, ExceptT(..), throwE)
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
import qualified Data.OpEnergy.Account.API.V1.BlockTimeStrikePublic as API
import qualified Data.OpEnergy.Account.API.V1.BlockTimeStrikeGuess as GuessAPI
import qualified Data.OpEnergy.Account.API.V1.PagingResult as API
import qualified Data.OpEnergy.Account.API.V1.FilterRequest as API
import           Data.OpEnergy.Account.API.V1.UUID
import           Data.OpEnergy.Account.API.V1.BlockTimeStrikeFilterClass

import           OpEnergy.ExceptMaybe
                   ( eitherLogThrowOrReturn
                   , runExceptPrefixT
                   , exceptTMaybeT
                   )
import           OpEnergy.PagingResult( pagingResult)
import           OpEnergy.Account.Server.V1.Config (Config(..))
import           OpEnergy.Account.Server.V1.Class ( AppT, AppM, State(..), profile, withDBTransaction)
import           OpEnergy.Account.Server.V1.AccountService (mgetPersonByAccountToken)
import           OpEnergy.Account.Server.V1.Person
import qualified OpEnergy.BlockTimeStrike.Server.V1.Class as BlockTime
import qualified OpEnergy.BlockTimeStrike.Server.V2.BlockSpanTimeStrikeFilter as BlockTimeStrikeFilter
import qualified OpEnergy.BlockTimeStrike.Server.V2.DBModel as DB
import           OpEnergy.BlockTimeStrike.Server.V1.SlowFast
import qualified OpEnergy.PagingResult as PagingResult

mgetBlockSpanTimeStrikeFuture
  :: (MonadIO m, MonadMonitor m)
  => BlockHeight
  -> Positive Int
  -> Natural Int
  -> AppT m (Maybe (Entity DB.BlockSpanTimeStrike))
mgetBlockSpanTimeStrikeFuture blockHeight spanSize strikeMediantime =
    let name = "mgetBlockTimeStrikeFuture"
    in profile name $ do
  mret <- withDBTransaction "" $ do
    selectFirst [ DB.BlockSpanTimeStrikeBlock ==. blockHeight
                , DB.BlockSpanTimeStrikeSpanSize ==. spanSize
                , DB.BlockSpanTimeStrikeMediantime ==. fromIntegral strikeMediantime
                ]
                []
  return $! do
    ret <- mret
    ret

-- | O(ln accounts).
-- Tries to create future block time strike. Requires authenticated user and blockheight should be in the future
createBlockSpanTimeStrikeFutureGuess
  :: API.AccountToken
  -> BlockHeight
  -> Positive Int
  -> Natural Int
  -> API.SlowFast
  -> AppM GuessAPI.BlockTimeStrikeGuessPublic
createBlockSpanTimeStrikeFutureGuess
    token blockHeight spanSize strikeMediantime guess =
    let name = "createBlockSpanTimeStrikeFutureGuess"
    in profile name $ eitherLogThrowOrReturn $ runExceptPrefixT name $ do
  configBlockTimeStrikeGuessMinimumBlockAheadCurrentTip <- lift $ asks
    $ configBlockTimeStrikeGuessMinimumBlockAheadCurrentTip . config
  latestConfirmedBlockV <- lift $ asks
    $ BlockTime.latestConfirmedBlock . blockTimeState
  tip <- exceptTMaybeT "there is no current tip yet"
    $ liftIO $ TVar.readTVarIO latestConfirmedBlockV
  when ( blockHeaderMediantime tip > fromIntegral strikeMediantime)
    $ throwE "strikeMediantime is in the past, which is not expected"
  when ( blockHeaderHeight tip
       + naturalFromPositive configBlockTimeStrikeGuessMinimumBlockAheadCurrentTip
         > blockHeight
       ) $ throwE "block height for new block time strike should be in the \
                  \future + minimum \
                  \configBlockTimeStrikeFutureGuessMinimumBlockAheadCurrentTip"
  Entity personKey person <- exceptTMaybeT
    "person was not able to authenticate itself"
    $ mgetPersonByAccountToken token
  Entity strikeKey strike <- exceptTMaybeT
    "future strike was not able to authenticate itself"
    $ mgetBlockSpanTimeStrikeFuture blockHeight spanSize strikeMediantime
  newGuess <- exceptTMaybeT "something went wrong"
    $ createBlockSpanTimeStrikeFutureGuess personKey strikeKey guess
  return $ GuessAPI.BlockTimeStrikeGuessPublic
    { GuessAPI.person = apiModelUUIDPerson $ personUuid person
    , GuessAPI.strike =
      DB.apiBlockTimeStrikeModelBlockSpanTimeStrike strike
    , GuessAPI.creationTime = DB.blockSpanTimeStrikeGuessCreationTime newGuess
    , GuessAPI.guess = guess
    }
  where
    createBlockSpanTimeStrikeFutureGuess
      :: (MonadMonitor m, MonadIO m)
      => Key Person
      -> Key DB.BlockSpanTimeStrike
      -> API.SlowFast
      -> AppT m (Maybe DB.BlockSpanTimeStrikeGuess)
    createBlockSpanTimeStrikeFutureGuess personKey strikeKey guess =
        let name = "createBlockSpanTimeStrikeFutureGuess"
        in profile "createBlockSpanTimeStrikeFutureGuess" $ do
      nowUTC <- liftIO getCurrentTime
      let now = utcTimeToPOSIXSeconds nowUTC
      withDBTransaction "" $ do
        let value = DB.BlockSpanTimeStrikeGuess
              { blockSpanTimeStrikeGuessIsFast = modelApiSlowFast guess
              , blockSpanTimeStrikeGuessCreationTime = now
              , blockSpanTimeStrikeGuessPerson = personKey
              , blockSpanTimeStrikeGuessStrike = strikeKey
              }
        _ <- insert value
        mguessesCount <- selectFirst
          [ DB.CalculatedBlockSpanTimeStrikeGuessesCountStrike ==. strikeKey
          ]
          []
        case mguessesCount of
          Just (Entity guessesCountId _) -> -- there is a record of precalculated counts, update it
            update guessesCountId
              [ DB.CalculatedBlockSpanTimeStrikeGuessesCountGuessesCount +=. verifyNatural 1
              ]
          Nothing -> do -- no record exist yet, recalculate to be idempotent TODO: maybe it is a bad place to recalculate and we need some kind of scheduled task for this
            guessesCount <- count [ DB.BlockSpanTimeStrikeGuessStrike ==. strikeKey ]
            void $! insert $! DB.CalculatedBlockSpanTimeStrikeGuessesCount
              { calculatedBlockSpanTimeStrikeGuessesCountGuessesCount =
                verifyNatural guessesCount
              , calculatedBlockSpanTimeStrikeGuessesCountStrike =
                strikeKey
              }
        return value

-- | O(ln accounts) + O(BlockTimeStrikePast).
-- returns list BlockTimeStrikePast records
getBlockSpanTimeStrikeGuessResultsPage
  :: Maybe (Natural Int)
  -> Maybe (API.FilterRequest API.BlockTimeStrikeGuess API.BlockTimeStrikeGuessResultPublicFilter)
  -> AppM (API.PagingResult API.BlockTimeStrikeGuessResultPublic)
getBlockSpanTimeStrikeGuessResultsPage mpage mfilterAPI =
    let name = "getBlockSpanTimeStrikeGuessResultsPage"
    in profile name $ eitherLogThrowOrReturn $ runExceptPrefixT name $ do
  mconfirmedBlockV <- lift $ asks
    $ BlockTime.latestConfirmedBlock . blockTimeState
  recordsPerReply <- lift $ asks $ configRecordsPerReply . config
  confirmedBlock <- exceptTMaybeT "no confirmed block found yet"
    $ liftIO $ TVar.readTVarIO mconfirmedBlockV
  let
      linesPerPage = maybe
        recordsPerReply
        ( fromMaybe recordsPerReply
        . API.blockTimeStrikeGuessResultPublicFilterLinesPerPage
        . fst
        . API.unFilterRequest
        )
        mfilter
  guessesTail <- exceptTMaybeT "something went wrong, check logs for details"
    $ withDBTransaction "" $ C.runConduit
      $ filters linesPerPage confirmedBlock
      .| ( C.drop (fromNatural page * fromPositive linesPerPage)
         >> C.awaitForever C.yield
         ) -- navigate to page
      .| C.map renderBlockTimeStrikeGuessResultPublic
      .| C.take (fromPositive linesPerPage + 1) -- we take +1 to understand if there is a next page available
  let
      newPage =
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
    mfilter = fmap DB.coerceFilterRequestBlockTimeStrikeGuess mfilterAPI
    sort = maybe Descend (API.sortOrder . API.unFilterRequest . id1 . API.mapFilter) mfilter
      where
        id1
          :: API.FilterRequest DB.BlockSpanTimeStrike API.BlockTimeStrikeGuessResultPublicFilter
          -> API.FilterRequest DB.BlockSpanTimeStrike API.BlockTimeStrikeGuessResultPublicFilter
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
         (Entity DB.BlockSpanTimeStrike)
         (ReaderT SqlBackend (NoLoggingT (ResourceT IO)))
         ()
    fetchConfirmedStrikes recordsPerReply confirmedBlock =
      PagingResult.pagingLoopSource
        Nothing
        recordsPerReply
        ((DB.BlockSpanTimeStrikeBlock <=. blockHeaderHeight confirmedBlock)
        : maybe
          []
          ( API.buildFilter . API.unFilterRequest . API.mapFilter)
          mfilter
        )
        sort
        DB.BlockSpanTimeStrikeId



-- | returns list BlockTimeStrikeGuess records
getBlockSpanTimeStrikesGuessesPage
  :: Maybe (Natural Int)
  -> Maybe (API.FilterRequest API.BlockTimeStrikeGuess API.BlockTimeStrikeGuessResultPublicFilter)
  -> AppM (API.PagingResult API.BlockTimeStrikeGuessResultPublic)
getBlockSpanTimeStrikesGuessesPage mpage mfilterAPI =
    let name = "getBlockSpanTimeStrikesGuessesPage"
    in profile name $ eitherLogThrowOrReturn $ runExceptPrefixT name $ do
  latestUnconfirmedBlockHeightV <- lift $ asks
    $ BlockTime.latestUnconfirmedBlockHeight . blockTimeState
  configBlockTimeStrikeGuessMinimumBlockAheadCurrentTip <- lift $ asks
    $ configBlockTimeStrikeGuessMinimumBlockAheadCurrentTip . config
  latestConfirmedBlockV <- lift $ asks
    $ BlockTime.latestConfirmedBlock . blockTimeState
  recordsPerReply <- lift $ asks $ configRecordsPerReply . config
  let
      linesPerPage = maybe
        recordsPerReply
        ( fromMaybe recordsPerReply
        . API.blockTimeStrikeGuessResultPublicFilterLinesPerPage
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
          ( maybe
            Nothing
            ( API.blockTimeStrikeGuessResultPublicFilterClass
            . fst
            . API.unFilterRequest
            )
            mfilter
          )
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
      DB.BlockSpanTimeStrikeGuessId
      $ streamGuessStrikeObservedResultAndOwner finalStrikesFilter linesPerPage
  where
    mfilter = fmap DB.coerceFilterRequestBlockTimeStrikeGuess mfilterAPI
    strikeFilter :: [Filter DB.BlockSpanTimeStrike]
    strikeFilter = maybe [] (API.buildFilter . API.unFilterRequest . API.mapFilter) mfilter
    guessFilter :: [ Filter DB.BlockSpanTimeStrikeGuess]
    guessFilter = maybe [] (API.buildFilter . API.unFilterRequest . API.mapFilter) mfilter
    sort = maybe Descend (API.sortOrder . API.unFilterRequest . id1 . API.mapFilter) mfilter
      where
        id1
          :: API.FilterRequest DB.BlockSpanTimeStrike API.BlockTimeStrikeGuessResultPublicFilter
          -> API.FilterRequest DB.BlockSpanTimeStrike API.BlockTimeStrikeGuessResultPublicFilter
        id1 = id -- helping typechecker

    streamGuessStrikeObservedResultAndOwner
      :: [Filter DB.BlockSpanTimeStrike]
      -> Positive Int
      -> ConduitT
           (Entity DB.BlockSpanTimeStrikeGuess)
           API.BlockTimeStrikeGuessResultPublic
           (ReaderT SqlBackend (NoLoggingT (ResourceT IO)))
           ()
    streamGuessStrikeObservedResultAndOwner finalFilter linesPerPage
      =  C.zipConcatMapMC fetchPerGuessBlockTimeStrikeByGuess
      .| C.mapM possiblyFetchObservedResultAndZipWithGuessAndStrike
      .| C.zipConcatMapMC fetchPerGuessOwnerAndZipObservedResultGuessAndStrike
      .| C.map reorgTupleToFitRenderBlockTimeGuessResultPublic
      .| C.map renderBlockTimeStrikeGuessResultPublic
      where
        -- unique
        fetchPerGuessBlockTimeStrikeByGuess
          :: Entity DB.BlockSpanTimeStrikeGuess
          -> ConduitT
             ()
             (Entity DB.BlockSpanTimeStrike)
             (ReaderT SqlBackend (NoLoggingT (ResourceT IO)))
             ()
        fetchPerGuessBlockTimeStrikeByGuess (Entity _ guess) =
          PagingResult.pagingLoopSource
            Nothing
            linesPerPage
            ((DB.BlockSpanTimeStrikeId ==. DB.blockSpanTimeStrikeGuessStrike guess)
            :finalFilter
            )
            sort
            DB.BlockSpanTimeStrikeId
        -- unique
        possiblyFetchObservedResultAndZipWithGuessAndStrike
          :: (Entity DB.BlockSpanTimeStrikeGuess, Entity DB.BlockSpanTimeStrike)
          -> (ReaderT SqlBackend (NoLoggingT (ResourceT IO)))
             (Entity DB.BlockSpanTimeStrikeGuess
             , Entity DB.BlockSpanTimeStrike
             , Maybe (Entity DB.BlockSpanTimeStrikeObserved)
             )
        possiblyFetchObservedResultAndZipWithGuessAndStrike
            (guess, strikeE@(Entity strikeId _)) = do
          case maybe
              Nothing
              (API.blockTimeStrikeGuessResultPublicFilterClass
              . fst
              . API.unFilterRequest
              )
              mfilter of
            Nothing -> do
              mObserved <- selectFirst
                [ DB.BlockSpanTimeStrikeObservedStrike ==. strikeId ]
                []
              return (guess, strikeE, mObserved) -- don't care about existence of observed data
            Just BlockTimeStrikeFilterClassGuessable-> do
              return (guess, strikeE, Nothing) -- strike has not been observed and finalFilter should ensure, that it is in the future with proper guess threshold
            Just BlockTimeStrikeFilterClassOutcomeUnknown-> do
              return (guess, strikeE, Nothing) -- hasn't been observed
            Just BlockTimeStrikeFilterClassOutcomeKnown-> do
              mObserved <- selectFirst
                [ DB.BlockSpanTimeStrikeObservedStrike ==. strikeId ]
                []
              return (guess, strikeE, mObserved) -- had been observed
        -- TODO: unique?
        fetchPerGuessOwnerAndZipObservedResultGuessAndStrike
          :: ( Entity DB.BlockSpanTimeStrikeGuess
             , Entity DB.BlockSpanTimeStrike
             , Maybe (Entity DB.BlockSpanTimeStrikeObserved)
             )
          -> ConduitT
             ()
             (Entity Person)
             (ReaderT SqlBackend (NoLoggingT (ResourceT IO)))
             ()
        fetchPerGuessOwnerAndZipObservedResultGuessAndStrike
            (Entity _ guess, _, _) =
          PagingResult.pagingLoopSource
            Nothing
            linesPerPage
            (( PersonId ==. DB.blockSpanTimeStrikeGuessPerson guess )
            : maybe
              []
              (API.buildFilter . API.unFilterRequest . API.mapFilter)
              mfilter
            )
            sort
            PersonId
        reorgTupleToFitRenderBlockTimeGuessResultPublic
          :: ( ( Entity DB.BlockSpanTimeStrikeGuess
               , Entity DB.BlockSpanTimeStrike
               , Maybe (Entity DB.BlockSpanTimeStrikeObserved)
               )
             , Entity Person
             )
          -> ( Entity DB.BlockSpanTimeStrike
             , Entity DB.BlockSpanTimeStrikeGuess
             , Entity Person
             , Maybe (Entity DB.BlockSpanTimeStrikeObserved)
             )
        reorgTupleToFitRenderBlockTimeGuessResultPublic
            ((guessE, strikeE, mObserved), personE) =
          (strikeE, guessE, personE, mObserved)


-- | returns list BlockTimeStrikeGuesses records
getBlockSpanTimeStrikeGuessesPage
  :: BlockHeight
  -> Natural Int
  -> Maybe (Natural Int)
  -> Maybe (API.FilterRequest API.BlockTimeStrikeGuess API.BlockTimeStrikeGuessResultPublicFilter)
  -> AppM (API.PagingResult API.BlockTimeStrikeGuessResultPublic)
getBlockSpanTimeStrikeGuessesPage blockHeight strikeMediantime mpage mfilterAPI =
    let name = "getBlockSpanTimeStrikeGuessesPage"
    in profile name $ eitherLogThrowOrReturn $ runExceptPrefixT name $ do
  recordsPerReply <- lift $ asks $ configRecordsPerReply . config
  let
      linesPerPage = maybe
                       recordsPerReply
                       ( fromMaybe recordsPerReply
                       . API.blockTimeStrikeGuessResultPublicFilterLinesPerPage
                       . fst
                       . API.unFilterRequest
                       )
                       mfilter
  guessesTail <- exceptTMaybeT "something went wrong, check logs for details"
    $ withDBTransaction "" $ do
      C.runConduit
        $ filters linesPerPage
        .| (C.drop (fromNatural page * fromPositive linesPerPage) >> C.awaitForever C.yield) -- navigate to page
        .| C.map renderBlockTimeStrikeGuessResultPublic
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
    mfilter = fmap DB.coerceFilterRequestBlockTimeStrikeGuess mfilterAPI
    sort = maybe Descend (API.sortOrder . API.unFilterRequest . id1 . API.mapFilter) mfilter
      where
        id1
          :: API.FilterRequest DB.BlockSpanTimeStrike API.BlockTimeStrikeGuessResultPublicFilter
          -> API.FilterRequest DB.BlockSpanTimeStrike API.BlockTimeStrikeGuessResultPublicFilter
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

-- | returns BlockTimeStrikeGuessPublic by strike and person, taken from account token
getBlockSpanTimeStrikeGuess
  :: API.AccountToken
  -> BlockHeight
  -> Natural Int
  -> AppM API.BlockTimeStrikeGuessResultPublic
getBlockSpanTimeStrikeGuess token blockHeight strikeMediantime =
    let name = "getBlockSpanTimeStrikeGuess"
    in profile name $ eitherLogThrowOrReturn $ runExceptPrefixT name $ do
  personE <- exceptTMaybeT "person was not able to authenticate itself"
    $ mgetPersonByAccountToken token
  mstrike <- exceptTMaybeT "something went wrong, see logs for details"
    $ actualGetStrikeGuess personE blockHeight (fromIntegral strikeMediantime)
  exceptTMaybeT "no strike or guess found"
    $ return mstrike
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
          .| C.map (renderBlockTimeStrikeGuessResultPublicByPerson person)
          .| C.head
    maybeFetchObservedStrike
      :: (Entity DB.BlockSpanTimeStrike, Entity DB.BlockSpanTimeStrikeGuess)
      -> ReaderT SqlBackend (NoLoggingT (ResourceT IO))
         ( Entity DB.BlockSpanTimeStrike
         , Entity DB.BlockSpanTimeStrikeGuess
         , Maybe (Entity DB.BlockSpanTimeStrikeObserved)
         )
    maybeFetchObservedStrike ( strikeE@(Entity strikeId _), guessE) = do
      mObserved <- selectFirst
        [ DB.BlockSpanTimeStrikeObservedStrike ==. strikeId ]
        []
      return (strikeE, guessE, mObserved)

getBlockSpanTimeStrikeGuessPerson
  :: UUID API.Person
  -> BlockHeight
  -> Natural Int
  -> AppM API.BlockTimeStrikeGuessResultPublic
getBlockSpanTimeStrikeGuessPerson uuid blockHeight strikeMediantime =
    let name = "getBlockSpanTimeStrikeGuessPerson"
    in profile name $ eitherLogThrowOrReturn $ runExceptPrefixT name $ do
  mperson <- exceptTMaybeT "something went wrong, check logs"
    $ mgetPersonByUUID uuid
  personE <- exceptTMaybeT "person was not able to authenticate itself"
    $ return mperson
  mguess <- exceptTMaybeT "something went wrong, see logs for details"
    $ actualGetStrikeGuess personE blockHeight (fromIntegral strikeMediantime)
  exceptTMaybeT "guess not found"
    $ return mguess
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
          .| C.map (renderBlockTimeStrikeGuessResultPublicByPerson person)
          .| C.head
    maybeFetchBlockTimeStrikeObserved
      :: (Entity DB.BlockSpanTimeStrike, Entity DB.BlockSpanTimeStrikeGuess)
      -> ReaderT
         SqlBackend
         (NoLoggingT (ResourceT IO))
         (Entity DB.BlockSpanTimeStrike
         , Entity DB.BlockSpanTimeStrikeGuess
         , Maybe (Entity DB.BlockSpanTimeStrikeObserved)
         )
    maybeFetchBlockTimeStrikeObserved (strikeE@(Entity strikeId _), guessE) = do
      mObserved <- selectFirst
        [ DB.BlockSpanTimeStrikeObservedStrike ==. strikeId]
        []
      return (strikeE, guessE, mObserved)

fetchBlockTimeStrikeByHeightAndMediantime
  :: Positive Int
  -> SortOrder
  -> BlockHeight
  -> POSIXTime
  -> ConduitT
     ()
     (Entity DB.BlockSpanTimeStrike)
     (ReaderT SqlBackend (NoLoggingT (ResourceT IO)))
     ()
fetchBlockTimeStrikeByHeightAndMediantime
    recordsPerReply sort blockHeight strikeMediantime =
  PagingResult.pagingLoopSource
    Nothing
    recordsPerReply
    [ DB.BlockSpanTimeStrikeBlock ==. blockHeight
    , DB.BlockSpanTimeStrikeMediantime ==. strikeMediantime
    ]
    sort
    DB.BlockSpanTimeStrikeId

renderBlockTimeStrikeGuessResultPublicByPerson
  :: Person
  -> ( Entity DB.BlockSpanTimeStrike
     , Entity DB.BlockSpanTimeStrikeGuess
     , Maybe (Entity DB.BlockSpanTimeStrikeObserved)
     )
  -> API.BlockTimeStrikeGuessResultPublic
renderBlockTimeStrikeGuessResultPublicByPerson person
    (Entity _ strike, Entity _ guess, mObserved) =
  API.BlockTimeStrikeGuessResultPublic
    { API.person = apiModelUUIDPerson $ personUuid person
    , API.strike = API.BlockTimeStrikePublic
      { API.blockTimeStrikePublicObservedResult = fmap
          (\(Entity _ result)->
            apiModelSlowFast $ DB.blockSpanTimeStrikeObservedIsFast result
          )
          mObserved
      , API.blockTimeStrikePublicObservedBlockMediantime = fmap
          (\(Entity _ result)->
            DB.blockSpanTimeStrikeObservedJudgementBlockMediantime result
          )
          mObserved
      , API.blockTimeStrikePublicObservedBlockHash = fmap
          (\(Entity _ result)->
            DB.blockSpanTimeStrikeObservedJudgementBlockHash result
          )
          mObserved
      , API.blockTimeStrikePublicObservedBlockHeight = fmap
          (\(Entity _ result)->
            DB.blockSpanTimeStrikeObservedJudgementBlockHeight result
          )
          mObserved
      , API.blockTimeStrikePublicBlock = DB.blockSpanTimeStrikeBlock strike
      , API.blockTimeStrikePublicStrikeMediantime =
        DB.blockSpanTimeStrikeMediantime strike
      , API.blockTimeStrikePublicCreationTime =
        DB.blockSpanTimeStrikeCreationTime strike
      }
    , API.creationTime = DB.blockSpanTimeStrikeGuessCreationTime guess
    , API.guess = apiModelSlowFast $ DB.blockSpanTimeStrikeGuessIsFast guess
    }

fetchBlockTimeStrikeGuessByStrike
  :: Positive Int
  -> SortOrder
  -> Maybe (API.FilterRequest DB.BlockSpanTimeStrikeGuess API.BlockTimeStrikeGuessResultPublicFilter)
  -> Entity DB.BlockSpanTimeStrike
  -> ConduitT
     ()
     (Entity DB.BlockSpanTimeStrikeGuess)
     (ReaderT SqlBackend (NoLoggingT (ResourceT IO)))
     ()
fetchBlockTimeStrikeGuessByStrike recordsPerReply sort mfilter (Entity strikeId _) = do
  PagingResult.pagingLoopSource
    Nothing
    recordsPerReply
    ( ( DB.BlockSpanTimeStrikeGuessStrike ==. strikeId )
    : maybe [] (API.buildFilter . API.unFilterRequest . API.mapFilter) mfilter
    )
    sort
    DB.BlockSpanTimeStrikeGuessId

fetchGuessPersonByBlockTimeStrikeGuess
  :: Positive Int
  -> SortOrder
  -> Maybe (API.FilterRequest DB.BlockSpanTimeStrikeGuess API.BlockTimeStrikeGuessResultPublicFilter)
  -> ( Entity DB.BlockSpanTimeStrike, Entity DB.BlockSpanTimeStrikeGuess)
  -> ConduitT
     ()
     (Entity Person)
     (ReaderT SqlBackend (NoLoggingT (ResourceT IO)))
     ()
fetchGuessPersonByBlockTimeStrikeGuess
    recordsPerReply sort mfilter (_, Entity _ guess ) = do
  PagingResult.pagingLoopSource
    Nothing
    recordsPerReply
    ( ( PersonId ==. DB.blockSpanTimeStrikeGuessPerson guess )
    : maybe [] (API.buildFilter . API.unFilterRequest . API.mapFilter) mfilter
    )
    sort
    PersonId

maybeFetchObservedStrikeByStrikeGuessAndPersonAndFlatten
  :: ( ( Entity DB.BlockSpanTimeStrike
       , Entity DB.BlockSpanTimeStrikeGuess
       )
     , Entity Person
     )
  -> ReaderT SqlBackend (NoLoggingT (ResourceT IO))
       ( Entity DB.BlockSpanTimeStrike
       , Entity DB.BlockSpanTimeStrikeGuess
       , Entity Person
       , Maybe (Entity DB.BlockSpanTimeStrikeObserved)
       )
maybeFetchObservedStrikeByStrikeGuessAndPersonAndFlatten
    ((strikeE@(Entity strikeId _), guessE), personE) = do
  mObserved <- selectFirst
    [ DB.BlockSpanTimeStrikeObservedStrike ==. strikeId]
    []
  return (strikeE, guessE, personE, mObserved)

renderBlockTimeStrikeGuessResultPublic
  :: ( Entity DB.BlockSpanTimeStrike
     , Entity DB.BlockSpanTimeStrikeGuess
     , Entity Person
     , Maybe (Entity DB.BlockSpanTimeStrikeObserved)
     )
  -> API.BlockTimeStrikeGuessResultPublic
renderBlockTimeStrikeGuessResultPublic
    (Entity _ strike, Entity _ guess, Entity _ person, mObserved) =
  API.BlockTimeStrikeGuessResultPublic
    { person = apiModelUUIDPerson $ personUuid person
    , strike = API.BlockTimeStrikePublic
      { blockTimeStrikePublicObservedResult = fmap
        (\(Entity _ result) -> apiModelSlowFast
          $ DB.blockSpanTimeStrikeObservedIsFast result
        )
        mObserved
      , blockTimeStrikePublicObservedBlockMediantime = fmap
        (\(Entity _ result) ->
          DB.blockSpanTimeStrikeObservedJudgementBlockMediantime result
        )
        mObserved
      , blockTimeStrikePublicObservedBlockHash = fmap
        (\(Entity _ result) ->
          DB.blockSpanTimeStrikeObservedJudgementBlockHash result
        )
        mObserved
      , blockTimeStrikePublicObservedBlockHeight = fmap
        (\(Entity _ result) ->
          DB.blockSpanTimeStrikeObservedJudgementBlockHeight result
        )
        mObserved
      , blockTimeStrikePublicBlock = DB.blockSpanTimeStrikeBlock strike
      , blockTimeStrikePublicStrikeMediantime =
        DB.blockSpanTimeStrikeMediantime strike
      , blockTimeStrikePublicCreationTime =
        DB.blockSpanTimeStrikeCreationTime strike
      }
    , creationTime = DB.blockSpanTimeStrikeGuessCreationTime guess
    , guess = apiModelSlowFast $ DB.blockSpanTimeStrikeGuessIsFast guess
    }

fetchBlockTimeStrikeGuessByStrikeAndPerson
  :: Positive Int
  -> PersonId
  -> Entity DB.BlockSpanTimeStrike
  -> ConduitT
     ()
     (Entity DB.BlockSpanTimeStrikeGuess)
     (ReaderT SqlBackend (NoLoggingT (ResourceT IO)))
     ()
fetchBlockTimeStrikeGuessByStrikeAndPerson
    recordsPerReply personKey (Entity strikeId _) = do
  PagingResult.pagingLoopSource
    Nothing
    recordsPerReply
    [ DB.BlockSpanTimeStrikeGuessStrike ==. strikeId
    , DB.BlockSpanTimeStrikeGuessPerson ==. personKey
    ]
    Descend
    DB.BlockSpanTimeStrikeGuessId

