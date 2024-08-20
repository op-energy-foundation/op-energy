{-- | This module implements BlockTime strike service.
 -}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE EmptyDataDecls          #-}
{-# LANGUAGE GADTs                     #-}
module OpEnergy.BlockTimeStrike.Server.V1.BlockTimeStrikeService
  ( createBlockTimeStrikeFuture
  , newTipHandlerLoop
  , getBlockTimeStrikesPage
  , getBlockTimeStrike
  ) where

import           Servant (err400, err500)
import           Control.Monad.Trans.Reader (ReaderT, ask, asks)
import           Control.Monad.Logger(NoLoggingT, logDebug, logError, logInfo)
import           Control.Monad(forever, void, when)
import           Data.Time.Clock(getCurrentTime)
import           Data.Time.Clock.POSIX(utcTimeToPOSIXSeconds, getPOSIXTime)
import qualified Control.Concurrent.STM.TVar as TVar
import qualified Control.Concurrent.MVar as MVar
import           Data.Text (Text)
import           Data.Conduit( (.|) )
import qualified Data.Conduit as C
import           Control.Monad.Trans

import           Database.Persist
import           Database.Persist.SqlBackend(SqlBackend)
import           Database.Persist.Pagination
import           Prometheus(MonadMonitor)
import           Control.Monad.Trans.Resource(ResourceT)
import           Servant.Client (BaseUrl)


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

-- | O(ln accounts).
-- Tries to create future block time strike. Requires authenticated user and blockheight should be in the future
createBlockTimeStrikeFuture :: AccountToken-> BlockHeight-> Natural Int-> AppM ()
createBlockTimeStrikeFuture token blockHeight strikeMediantime = profile "createBlockTimeStrike" $ do
  State{ config = Config{ configBlockTimeStrikeMinimumBlockAheadCurrentTip = configBlockTimeStrikeMinimumBlockAheadCurrentTip}
       , blockTimeState = BlockTime.State{ latestUnconfirmedBlockHeight = latestUnconfirmedBlockHeightV }
       } <- ask
  mlatestUnconfirmedBlockHeight <- liftIO $ TVar.readTVarIO latestUnconfirmedBlockHeightV
  case mlatestUnconfirmedBlockHeight of
    Nothing -> do
      let err = "ERROR: createBlockTimeStrike: there is no current observed tip yet"
      runLogging $ $(logError) err
      throwJSON err400 err
    Just tip
      | tip > fromIntegral strikeMediantime -> do
        let err = "ERROR: strikeMediantime is in the past, which is not expected"
        runLogging $ $(logError) err
        throwJSON err400 err
    Just tip
      | tip + naturalFromPositive configBlockTimeStrikeMinimumBlockAheadCurrentTip > blockHeight -> do
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
        }

data BlockObserved
data BlockMediantimeReachedBlockNotObserved
newtype Context a b = Context b
-- | this function is an entry point for a process, that creates blocktime past strikes when such block is being
-- confirmed. After BlockTimeStrikePast creation, it will add record to the BlockTimeStrikeFutureObservedBlock table
-- in order to notify hndlers in the chain (currently only guess game), which should move appropriate references
-- to such blocktime future strike into past strikes. Then, those subsequent handlers should create
-- BlockTimeStrikeFutureReadyToRemove record, which should be handled by archiveFutureStrikesLoop function
newTipHandlerLoop :: (MonadIO m, MonadMonitor m) => AppT m ()
newTipHandlerLoop = forever $ do
  State{ blockTimeState = BlockTime.State
         { blockTimeStrikeConfirmedTip = confirmedTipV
         }
       } <- ask
  -- get new confirmed tip notification from upstream handler
  confirmedTip <- liftIO $ MVar.takeMVar confirmedTipV
  runLogging $ $(logInfo) $ "BlockTimeStrikeService: tipHandler: received new confirmed tip height: " <> (tshow $ blockHeaderHeight confirmedTip)
  -- find out any future strikes <= new confirmed tip
  profile "newTipHandlerIteration" $ do
    _ <- observeStrikesByBlockHeight confirmedTip
    BlockTimeScheduledStrikeCreation.maybeCreateStrikes confirmedTip
  where
    -- | search strikes with block height < confirmed block and observe them
    observeStrikesByBlockHeight :: (MonadIO m, MonadMonitor m) => BlockHeader -> AppT m ()
    observeStrikesByBlockHeight confirmedBlock = profile "observeStrikesByBlockHeight" $ do
      blockspanURL <- asks (configBlockspanURL . config)
      recordsPerReply <- asks (configRecordsPerReply . config)
      state <- ask
      _ <- withDBTransaction "byBlockHeight" $ do
        let
            isStrikeBlockConfirmed =
              BlockTimeStrikeBlock <=. blockHeaderHeight confirmedBlock
            isStrikeMediantimeObserved =
              BlockTimeStrikeStrikeMediantime
              <=. ( fromIntegral $! blockHeaderMediantime confirmedBlock)
        C.runConduit
          $ streamEntities
            (   [isStrikeBlockConfirmed]
            ||. [isStrikeMediantimeObserved]
            )
            BlockTimeStrikeId
            (PageSize (fromPositive recordsPerReply))
            Descend
            (Range Nothing Nothing)
          .| ( C.awaitForever $ \strikeE@(Entity strikeId _) -> do -- check if there
                                   -- all the data had been discovered already
               let isStrikeOutcomeObserved =
                     BlockTimeStrikeObservedStrike ==. strikeId
               isStrikeOutcomeNotObserved <- fmap not $ lift $ exists
                 [ isStrikeOutcomeObserved
                 ]
               when isStrikeOutcomeNotObserved $ C.yield strikeE
             )
          .| ( do
               let
                   loop (cnt::Int) = do
                     mv <- C.await
                     case mv of
                       Nothing -> do
                         liftIO $ runLoggingIO state $ $(logDebug)
                           ("calculated results for " <> tshow cnt <> " strikes")
                       Just strikeE -> do
                         case eitherStrikeObservedByBlockOrMediantime strikeE of
                           Left blockObserved -> do
                             observeStrikeByBlockHeight blockspanURL blockObserved
                             loop $! (cnt + 1)
                           Right mediantimeReached -> do
                             observeStrikeByStrikeMediantime blockspanURL mediantimeReached
                             loop $! (cnt + 1)
               loop 0
             )
      return ()
      where
      eitherStrikeObservedByBlockOrMediantime
        :: (Entity BlockTimeStrike)
        -> Either (Context BlockObserved (Entity BlockTimeStrike))
                  (Context BlockMediantimeReachedBlockNotObserved (Entity BlockTimeStrike))
      eitherStrikeObservedByBlockOrMediantime strikeE@(Entity _ strike) =
        let
            isStrikeBlockConfirmed =
              blockTimeStrikeBlock strike
                <= blockHeaderHeight confirmedBlock
            isStrikeMediantimeObserved =
              blockTimeStrikeStrikeMediantime strike
                <= ( fromIntegral $! blockHeaderMediantime confirmedBlock)
        in case () of
          _ | isStrikeBlockConfirmed ->
              Left (Context strikeE)
          _ | isStrikeMediantimeObserved ->
              Right (Context strikeE)
          _ -> error "eitherStrikeObservedByBlockOrMediantime: impossible happend"
      observeStrikeByBlockHeight
        :: BaseUrl
        -> Context BlockObserved (Entity BlockTimeStrike)
        -> C.ConduitT
           (Entity BlockTimeStrike)
           C.Void
           (ReaderT SqlBackend (NoLoggingT (ResourceT IO)))
           ()
      observeStrikeByBlockHeight blockspanURL (Context (Entity strikeId strike)) = do
        judgementBlock <- do
          let isWeHaveStrikeBlockHeader = blockHeaderHeight confirmedBlock == blockTimeStrikeBlock strike
          if isWeHaveStrikeBlockHeader
            then return confirmedBlock
            else liftIO $! Blockspan.withClient blockspanURL
              $ getBlockByHeight (blockTimeStrikeBlock strike)
        now <- liftIO getPOSIXTime
        void $ lift $ insert $ BlockTimeStrikeObserved
          { blockTimeStrikeObservedStrike = strikeId
          , blockTimeStrikeObservedJudgementBlockHash =
            blockHeaderHash judgementBlock
          , blockTimeStrikeObservedJudgementBlockMediantime =
            fromIntegral (blockHeaderMediantime judgementBlock)
          , blockTimeStrikeObservedCreationTime = now
          , blockTimeStrikeObservedIsFast =
              ( if fromIntegral (blockHeaderMediantime judgementBlock)
                   <= blockTimeStrikeStrikeMediantime strike
                then Fast
                else Slow
              )
          , blockTimeStrikeObservedJudgementBlockHeight =
            blockHeaderHeight judgementBlock
          }
      observeStrikeByStrikeMediantime
        :: BaseUrl
        -> Context BlockMediantimeReachedBlockNotObserved (Entity BlockTimeStrike)
        -> C.ConduitT
           (Entity BlockTimeStrike)
           C.Void
           (ReaderT SqlBackend (NoLoggingT (ResourceT IO)))
           ()
      observeStrikeByStrikeMediantime blockspanURL (Context (Entity strikeId strike)) = do
        judgementBlock <- do
          let findFirstBlockWithMediantimeNextAfterStrikeMediantimeInLoop blockWithMediantimeMoreThanStrikeMediantime
                | blockHeaderHeight blockWithMediantimeMoreThanStrikeMediantime < 1 = return blockWithMediantimeMoreThanStrikeMediantime
                | otherwise = do
                  let
                      prevBlockHeight =
                        blockHeaderHeight blockWithMediantimeMoreThanStrikeMediantime - 1
                  prevBlock <- liftIO $! Blockspan.withClient blockspanURL
                    $ getBlockByHeight prevBlockHeight
                  let
                      isPrevBlockMediantimeLessThanStrikeMediantime =
                        fromIntegral (blockHeaderMediantime prevBlock)
                          < blockTimeStrikeStrikeMediantime strike
                      isPrevBlockMediantimeTheSameAsStrikeMediantime =
                        fromIntegral (blockHeaderMediantime prevBlock)
                        == blockTimeStrikeStrikeMediantime strike
                  case () of
                    _ | isPrevBlockMediantimeLessThanStrikeMediantime -> return blockWithMediantimeMoreThanStrikeMediantime
                    _ | isPrevBlockMediantimeTheSameAsStrikeMediantime-> return prevBlock
                    _ -> findFirstBlockWithMediantimeNextAfterStrikeMediantimeInLoop prevBlock
          findFirstBlockWithMediantimeNextAfterStrikeMediantimeInLoop confirmedBlock
        now <- liftIO getPOSIXTime
        let
            strikeOutcomeWhenStrikeBlockNotObservedYet = Slow
        void $ lift $ insert $ BlockTimeStrikeObserved
          { blockTimeStrikeObservedStrike = strikeId
          , blockTimeStrikeObservedJudgementBlockHash =
            blockHeaderHash judgementBlock
          , blockTimeStrikeObservedJudgementBlockMediantime =
            fromIntegral (blockHeaderMediantime judgementBlock)
          , blockTimeStrikeObservedCreationTime = now
          , blockTimeStrikeObservedIsFast = strikeOutcomeWhenStrikeBlockNotObservedYet
          , blockTimeStrikeObservedJudgementBlockHeight =
            blockHeaderHeight judgementBlock
          }

-- | returns list of BlockTimeStrikePublic records
getBlockTimeStrikesPage
  :: Maybe (Natural Int)
  -> Maybe (FilterRequest BlockTimeStrike BlockTimeStrikeFilter)
  -> AppM (PagingResult BlockTimeStrikeWithGuessesCountPublic)
getBlockTimeStrikesPage mpage mfilter = profile "getBlockTimeStrikesPage" $ do
  latestUnconfirmedBlockHeightV <- asks (BlockTime.latestUnconfirmedBlockHeight . blockTimeState)
  mlatestUnconfirmedBlockHeight <- liftIO $ TVar.readTVarIO latestUnconfirmedBlockHeightV
  configBlockTimeStrikeGuessMinimumBlockAheadCurrentTip <- asks (configBlockTimeStrikeGuessMinimumBlockAheadCurrentTip . config)
  case mlatestUnconfirmedBlockHeight of
    Just latestUnconfirmedBlockHeight -> do
      let finalFilter =
            case maybe Nothing (blockTimeStrikeFilterClass . fst . unFilterRequest) mfilter of
              Nothing -> filter
              Just BlockTimeStrikeFilterClassGuessable ->
                let
                    minimumGuessableBlock = latestUnconfirmedBlockHeight + naturalFromPositive configBlockTimeStrikeGuessMinimumBlockAheadCurrentTip
                in
                  ( (BlockTimeStrikeBlock >=. minimumGuessableBlock) -- block height should match threshold
                  :filter
                  )
              Just BlockTimeStrikeFilterClassOutcomeKnown -> filter -- class will be handled inside lookup routine
              Just BlockTimeStrikeFilterClassOutcomeUnknown -> filter -- class will be handled inside lookup routine
      mret <- getBlockTimeStrikePast finalFilter
      case mret of
        Nothing -> do
          throwJSON err500 ("something went wrong"::Text)
        Just ret -> return ret
    _ -> do
      let msg = "getBlockTimeStrikesPage: no confirmed block found yet"
      runLogging $  $(logError) msg
      throwJSON err500 msg
  where
    sort = maybe Descend (sortOrder . unFilterRequest) mfilter
    filter = (maybe [] (buildFilter . unFilterRequest) mfilter)
    getBlockTimeStrikePast finalFilter = do
      recordsPerReply <- asks (configRecordsPerReply . config)
      let
          linesPerPage = maybe recordsPerReply (maybe recordsPerReply id . blockTimeStrikeFilterLinesPerPage . fst . unFilterRequest ) mfilter
      let
          eGuessesCount = case (maybe StrikeSortOrderDescend (maybe StrikeSortOrderDescend id . blockTimeStrikeFilterSort . fst . unFilterRequest) mfilter) of
            StrikeSortOrderAscend -> Left ()
            StrikeSortOrderDescend -> Left ()
            StrikeSortOrderAscendGuessesCount ->  Right ()
            StrikeSortOrderDescendGuessesCount -> Right ()
      case eGuessesCount of
        Left () -> pagingResult mpage linesPerPage finalFilter sort BlockTimeStrikeId -- select strikes with given filter first
          $ ( C.awaitForever $ \v@(Entity strikeId _) -> do -- class
              -- now get possible observed data for strike
              mObserved <- lift $ selectFirst
                ( (BlockTimeStrikeObservedStrike ==. strikeId)
                : (maybe [] ( buildFilter . unFilterRequest . mapFilter) mfilter)
                )
                []
              -- now get possible calculated outcome data for strike
              mResult <- lift $ selectFirst
                ( (BlockTimeStrikeObservedStrike ==. strikeId)
                : []
                )
                []
              case maybe Nothing (blockTimeStrikeFilterClass . fst . unFilterRequest) mfilter of
                Nothing -> C.yield (v, mObserved, mResult) -- don't care about existence of observed data
                Just BlockTimeStrikeFilterClassGuessable-> case mResult of
                  Nothing -> C.yield (v, mObserved, mResult) -- strike have not been observed and finalFilter should ensure, that it is in the future with proper guess threshold
                  _ -> return () -- otherwise, block haven't matched the criteria and should be ignored
                Just BlockTimeStrikeFilterClassOutcomeUnknown-> case mResult of
                  Nothing-> C.yield (v, mObserved, mResult) -- haven't been observed
                  _ -> return ()
                Just BlockTimeStrikeFilterClassOutcomeKnown-> case mResult of
                  Just _ -> C.yield (v, mObserved, mResult) -- had been observed
                  _ -> return ()
            )
          .| ( C.awaitForever $ \(Entity strikeId strike, mObserved, mResult) -> do
              mguessesCount <- lift $ selectFirst
                [ CalculatedBlockTimeStrikeGuessesCountStrike ==. strikeId]
                []
              guessesCount <- case mguessesCount of
                Just (Entity _ guessesCount) -> -- results are already calculated
                  return (calculatedBlockTimeStrikeGuessesCountGuessesCount guessesCount)
                Nothing -> do -- fallback mode, recount online, which maybe a bad thing to do here TODO decide if it should be removed
                  guessesCount <- lift $ verifyNatural <$> count [ BlockTimeStrikeGuessStrike ==. strikeId ]
                  _ <- lift $ insert $! CalculatedBlockTimeStrikeGuessesCount
                    { calculatedBlockTimeStrikeGuessesCountStrike = strikeId
                    , calculatedBlockTimeStrikeGuessesCountGuessesCount = guessesCount
                    }
                  return guessesCount
              C.yield (BlockTimeStrikeWithGuessesCountPublic
                       { blockTimeStrikeWithGuessesCountPublicStrike = BlockTimeStrikePublic
                         { blockTimeStrikePublicObservedResult = maybe
                           Nothing
                           (\(Entity _ v)-> Just $! blockTimeStrikeObservedIsFast v)
                           mResult
                         , blockTimeStrikePublicObservedBlockMediantime = maybe
                           Nothing
                           (\(Entity _ v)-> Just $! blockTimeStrikeObservedJudgementBlockMediantime v)
                           mObserved
                         , blockTimeStrikePublicObservedBlockHash = maybe
                           Nothing
                           (\(Entity _ v)-> Just $! blockTimeStrikeObservedJudgementBlockHash v)
                           mObserved
                         , blockTimeStrikePublicObservedBlockHeight = maybe
                           Nothing
                           (\(Entity _ v)-> Just $! blockTimeStrikeObservedJudgementBlockHeight v)
                           mObserved
                         , blockTimeStrikePublicBlock = blockTimeStrikeBlock strike
                         , blockTimeStrikePublicStrikeMediantime = blockTimeStrikeStrikeMediantime strike
                         , blockTimeStrikePublicCreationTime = blockTimeStrikeCreationTime strike
                         }
                       , blockTimeStrikeWithGuessesCountPublicGuessesCount = fromIntegral guessesCount
                       }
                      )
            )
        Right () -> pagingResult mpage linesPerPage [] sort CalculatedBlockTimeStrikeGuessesCountGuessesCount
          $ ( C.awaitForever $ \(Entity _ guessesCount) -> do
              mstrike <- lift $ selectFirst
                ((BlockTimeStrikeId ==. calculatedBlockTimeStrikeGuessesCountStrike guessesCount)
                 :finalFilter
                )
                []
              mObserved <- lift $ selectFirst
                ( (BlockTimeStrikeObservedStrike ==. calculatedBlockTimeStrikeGuessesCountStrike guessesCount)
                : (maybe [] ( buildFilter . unFilterRequest . mapFilter) mfilter)
                )[]
              case mstrike of
                Just (Entity _ strike) -> C.yield (BlockTimeStrikeWithGuessesCountPublic
                       { blockTimeStrikeWithGuessesCountPublicStrike = BlockTimeStrikePublic
                         { blockTimeStrikePublicObservedResult = maybe
                           Nothing
                           (\(Entity _ v)-> Just $! blockTimeStrikeObservedIsFast v)
                           mObserved
                         , blockTimeStrikePublicObservedBlockMediantime = maybe
                           Nothing
                           (\(Entity _ v)-> Just $! blockTimeStrikeObservedJudgementBlockMediantime v)
                           mObserved
                         , blockTimeStrikePublicObservedBlockHash = maybe
                           Nothing
                           (\(Entity _ v)-> Just $! blockTimeStrikeObservedJudgementBlockHash v)
                           mObserved
                         , blockTimeStrikePublicObservedBlockHeight = maybe
                           Nothing
                           (\(Entity _ v)-> Just $! blockTimeStrikeObservedJudgementBlockHeight v)
                           mObserved
                         , blockTimeStrikePublicBlock = blockTimeStrikeBlock strike
                         , blockTimeStrikePublicStrikeMediantime = blockTimeStrikeStrikeMediantime strike
                         , blockTimeStrikePublicCreationTime = blockTimeStrikeCreationTime strike
                         }
                       , blockTimeStrikeWithGuessesCountPublicGuessesCount =
                         fromIntegral (calculatedBlockTimeStrikeGuessesCountGuessesCount guessesCount)
                       }
                      )
                Nothing -> return ()
            )

-- | returns BlockTimeStrikePublic records
getBlockTimeStrike
  :: BlockHeight
  -> Natural Int
  -> AppM BlockTimeStrikeWithGuessesCountPublic
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
                         mguessesCount <- lift $ selectFirst
                           [ CalculatedBlockTimeStrikeGuessesCountStrike ==. strikeId]
                           []
                         guessesCount <- case mguessesCount of
                           Just (Entity _ guessesCount) -> -- results are already calculated
                             return (calculatedBlockTimeStrikeGuessesCountGuessesCount guessesCount)
                           Nothing -> do -- fallback mode, recount online, which maybe a bad thing to do here TODO decide if it should be removed
                             guessesCount <- lift $ verifyNatural <$> count [ BlockTimeStrikeGuessStrike ==. strikeId ]
                             _ <- lift $ insert $! CalculatedBlockTimeStrikeGuessesCount
                               { calculatedBlockTimeStrikeGuessesCountStrike = strikeId
                               , calculatedBlockTimeStrikeGuessesCountGuessesCount = guessesCount
                               }
                             return guessesCount
                         mObserved <- lift $ selectFirst
                           [ BlockTimeStrikeObservedStrike ==. strikeId ]
                           []
                         return ( Just ( BlockTimeStrikeWithGuessesCountPublic
                                         { blockTimeStrikeWithGuessesCountPublicStrike = BlockTimeStrikePublic
                                           { blockTimeStrikePublicObservedResult = maybe
                                             Nothing
                                             (\(Entity _ v)-> Just $! blockTimeStrikeObservedIsFast v)
                                             mObserved
                                           , blockTimeStrikePublicObservedBlockMediantime = maybe
                                             Nothing
                                             (\(Entity _ v)-> Just $! blockTimeStrikeObservedJudgementBlockMediantime v)
                                             mObserved
                                           , blockTimeStrikePublicObservedBlockHash = maybe
                                             Nothing
                                             (\(Entity _ v)-> Just $! blockTimeStrikeObservedJudgementBlockHash v)
                                             mObserved
                                           , blockTimeStrikePublicObservedBlockHeight = maybe
                                             Nothing
                                             (\(Entity _ v)-> Just $! blockTimeStrikeObservedJudgementBlockHeight v)
                                             mObserved
                                           , blockTimeStrikePublicBlock = blockTimeStrikeBlock strike
                                           , blockTimeStrikePublicStrikeMediantime = blockTimeStrikeStrikeMediantime strike
                                           , blockTimeStrikePublicCreationTime = blockTimeStrikeCreationTime strike
                                           }
                                         , blockTimeStrikeWithGuessesCountPublicGuessesCount =
                                           fromIntegral guessesCount
                                         }
                                       )
                                )
                       Nothing -> return acc
               in loop Nothing
             )

