{-- | This module implements BlockTime strike service.
 -}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE EmptyDataDecls          #-}
{-# LANGUAGE GADTs                     #-}
module OpEnergy.BlockTimeStrike.Server.V1.BlockTimeStrikeService
  ( createBlockTimeStrikeFutureHandler
  , createBlockTimeStrikeFuture
  , newTipHandlerLoop
  , getBlockTimeStrike
  , getBlockTimeStrikeHandler
  , module OpEnergy.BlockTimeStrike.Server.V1.BlockTimeStrikeService.GetBlockTimeStrikesPage
  ) where

import           Servant ( err500, err400, ServerError)
import           Control.Monad.Trans.Reader ( ask, asks, ReaderT)
import           Control.Monad.Logger( logError, logInfo, NoLoggingT)
import           Control.Monad(forever, when, void)
import           Data.Time.Clock(getCurrentTime)
import           Data.Time.Clock.POSIX(utcTimeToPOSIXSeconds)
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TVar as TVar
import qualified Control.Concurrent.MVar as MVar
import           Data.Text (Text)
import           Data.Conduit( (.|) )
import qualified Data.Conduit as C
import qualified Data.Conduit.List as C
import           Control.Monad.Trans
import           Control.Monad.Trans.Except
                   ( runExceptT, ExceptT (..), throwE)
import           Control.Monad.Trans.Resource( ResourceT)

import           Database.Persist
import           Database.Persist.Sql
import           Database.Persist.Pagination
import           Prometheus(MonadMonitor)


import           Data.Text.Show (tshow)
import qualified Data.OpEnergy.Account.API.V1.Account as API
import           Data.OpEnergy.API.V1.Block
import           Data.OpEnergy.API.V1.Natural
import           Data.OpEnergy.API.V1.Positive( naturalFromPositive, fromPositive)
import qualified Data.OpEnergy.Account.API.V1.BlockTimeStrike            as API

import           OpEnergy.ExceptMaybe(exceptTMaybeT)
import           OpEnergy.Error( eitherThrowJSON, runExceptPrefixT)
import qualified OpEnergy.BlockTimeStrike.Server.V1.Class as BlockTime
import qualified OpEnergy.BlockTimeStrike.Server.V1.BlockTimeScheduledStrikeCreation
                 as BlockTimeScheduledStrikeCreation
import qualified OpEnergy.BlockTimeStrike.Server.V1.BlockTimeStrikeObserve as Observe
import qualified OpEnergy.BlockTimeStrike.Server.V1.Context as Context
import           OpEnergy.BlockTimeStrike.Server.V1.BlockTimeStrike
import           OpEnergy.BlockTimeStrike.Server.V1.BlockTimeStrikeGuess
import qualified OpEnergy.BlockTimeStrike.Server.V1.SlowFast as SlowFast
import           OpEnergy.BlockTimeStrike.Server.V1.BlockTimeStrikeService.GetBlockTimeStrikesPage
import           OpEnergy.Account.Server.V1.Config
                 as Config(Config(..))
import           OpEnergy.Account.Server.V1.AccountService (mgetPersonByAccountToken)
import           OpEnergy.Account.Server.V1.Class (AppT, AppM, State(..), runLogging, profile, withDBTransaction )
import           OpEnergy.Account.Server.V1.Person

-- | O(ln accounts).
-- Tries to create future block time strike. Requires authenticated user and blockheight should be in the future
createBlockTimeStrikeFutureHandler
  :: API.AccountToken
  -> BlockHeight
  -> Natural Int
  -> AppM ()
createBlockTimeStrikeFutureHandler token blockHeight strikeMediantime =
    let name = "createBlockTimeStrikeFuture"
    in profile name $ do
  eitherThrowJSON
    (\reason-> do
      callstack <- asks callStack
      let msg = callstack <> ": " <> reason
      runLogging $ $(logError) msg
      return msg
    )
    $ runExceptPrefixT name $ do
    void $ ExceptT $ createBlockTimeStrikeFuture token blockHeight
      strikeMediantime

createBlockTimeStrikeFuture
  :: API.AccountToken
  -> BlockHeight
  -> Natural Int
  -> AppM (Either (ServerError, Text) BlockTimeStrike)
createBlockTimeStrikeFuture token blockHeight strikeMediantime =
    let name = "createBlockTimeStrikeFuture"
    in profile name $ runExceptPrefixT name $ do
  configBlockTimeStrikeMinimumBlockAheadCurrentTip <- lift $ asks
    $ Config.configBlockTimeStrikeMinimumBlockAheadCurrentTip . config
  latestUnconfirmedBlockHeightV <- lift $ asks (BlockTime.latestUnconfirmedBlockHeight . blockTimeState)
  latestConfirmedBlockV <- lift $ asks
    $ BlockTime.latestConfirmedBlock . blockTimeState
  (tip, latestConfirmedBlock) <-
    ExceptT $ liftIO $ STM.atomically $ runExceptT $ (,)
      <$> (exceptTMaybeT (err500, "latest unconfirmed block hasn't been received yet")
          $ TVar.readTVar latestUnconfirmedBlockHeightV
          )
      <*> (exceptTMaybeT (err500, "latest confirmed block hasn't been received yet")
          $ TVar.readTVar latestConfirmedBlockV
          )
  when (blockHeaderMediantime latestConfirmedBlock >= fromIntegral strikeMediantime) $
    throwE ( err400, "strikeMediantime is in the past, which is not expected")
  when ( tip + naturalFromPositive configBlockTimeStrikeMinimumBlockAheadCurrentTip
         > blockHeight
       ) $ throwE (err400, "block height for new block time strike should be in the \
           \future + minimum configBlockTimeStrikeMinimumBlockAheadCurrentTip")
  person <- exceptTMaybeT (err400, "person was not able to authenticate itself")
    $ mgetPersonByAccountToken token
  exceptTMaybeT (err500, "something went wrong")
    $ createBlockTimeStrikeEnsuredConditions person
  where
    createBlockTimeStrikeEnsuredConditions
      :: (MonadIO m, MonadMonitor m)
      => Entity Person
      -> AppT m (Maybe BlockTimeStrike)
    createBlockTimeStrikeEnsuredConditions _ = do
      nowUTC <- liftIO getCurrentTime
      let
          now = utcTimeToPOSIXSeconds nowUTC
          record = BlockTimeStrike
            { blockTimeStrikeBlock = blockHeight
            , blockTimeStrikeStrikeMediantime = fromIntegral strikeMediantime
            , blockTimeStrikeCreationTime = now
            }
      withDBTransaction "" $ do
        strikeId <- insert record
        insert_
          ( CalculatedBlockTimeStrikeGuessesCount
          { calculatedBlockTimeStrikeGuessesCountStrike = strikeId
          , calculatedBlockTimeStrikeGuessesCountGuessesCount = 0
          , calculatedBlockTimeStrikeGuessesCountFastCount = 0
          , calculatedBlockTimeStrikeGuessesCountSlowCount = 0
          }
          )
        return record

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
    Observe.withLeastUnobservedConfirmedBlock confirmedTip $ \leastUnobservedConfirmedBlock-> do
      Observe.observeStrikes leastUnobservedConfirmedBlock
      BlockTimeScheduledStrikeCreation.maybeCreateStrikes $! Context.unContext leastUnobservedConfirmedBlock

getBlockTimeStrikeHandler
  :: BlockHeight
  -> Natural Int
  -> AppM API.BlockTimeStrikeWithGuessesCount
getBlockTimeStrikeHandler blockHeight strikeMediantime =
    let name = "V1.BlockTimeStrikeService.getBlockTimeStrikeHandler"
    in profile name $ eitherThrowJSON
  (\reason-> do
    callstack <- asks callStack
    let msg = callstack <> ": " <> reason
    runLogging $ $(logError) msg
    return msg
  )
  $ getBlockTimeStrike blockHeight strikeMediantime

-- | returns BlockTimeStrike records
getBlockTimeStrike
  :: BlockHeight
  -> Natural Int
  -> AppM (Either (ServerError, Text) API.BlockTimeStrikeWithGuessesCount)
getBlockTimeStrike blockHeight strikeMediantime =
    let name = "getBlockTimeStrike"
    in profile name $ runExceptPrefixT name $ do
  recordsPerReply <- lift $ asks (configRecordsPerReply . config)
  mStrike <- exceptTMaybeT (err500, "something went wrong")
    $ withDBTransaction "" $ do
        C.runConduit
          $ streamEntities
            [ BlockTimeStrikeBlock ==. blockHeight, BlockTimeStrikeStrikeMediantime ==. fromIntegral strikeMediantime ]
            BlockTimeStrikeId
            (PageSize (fromPositive recordsPerReply + 1))
            Descend
            (Range Nothing Nothing)
          .| C.mapM getGuessesCountByBlockTimeStrike
          .| C.mapM maybeFetchObserved
          .| C.map renderBlockTimeStrikeWithGuessesCount
          .| C.head
  exceptTMaybeT (err400, "strike not found") $ return mStrike
  where
    getGuessesCountByBlockTimeStrike
      :: Entity BlockTimeStrike
      -> ReaderT SqlBackend (NoLoggingT (ResourceT IO))
         (Entity BlockTimeStrike, Natural Int)
    getGuessesCountByBlockTimeStrike strikeE@(Entity strikeId _) = do
      mguessesCount <- selectFirst
        [ CalculatedBlockTimeStrikeGuessesCountStrike ==. strikeId]
        []
      let guessesCount = maybe
            0
            (\(Entity _ v) -> calculatedBlockTimeStrikeGuessesCountGuessesCount v)
            mguessesCount
      return (strikeE, guessesCount)
    maybeFetchObserved
      :: (Entity BlockTimeStrike, Natural Int)
      -> ReaderT SqlBackend (NoLoggingT (ResourceT IO))
         (Entity BlockTimeStrike, Natural Int, Maybe (Entity BlockTimeStrikeObserved))
    maybeFetchObserved (strikeE@(Entity strikeId _), guessesCount) = do
      mObserved <- selectFirst
        [ BlockTimeStrikeObservedStrike ==. strikeId ]
        []
      return (strikeE, guessesCount, mObserved)
    renderBlockTimeStrikeWithGuessesCount
      :: (Entity BlockTimeStrike, Natural Int, Maybe (Entity BlockTimeStrikeObserved))
      -> API.BlockTimeStrikeWithGuessesCount
    renderBlockTimeStrikeWithGuessesCount
        (Entity _ strike, guessesCount, mObserved) =
      API.BlockTimeStrikeWithGuessesCount
        { blockTimeStrikeWithGuessesCountStrike = API.BlockTimeStrike
          { blockTimeStrikeObservedResult = fmap
            (\(Entity _ v)-> SlowFast.apiModel $ blockTimeStrikeObservedIsFast v)
            mObserved
          , blockTimeStrikeObservedBlockMediantime = fmap
            (\(Entity _ v)-> blockTimeStrikeObservedJudgementBlockMediantime v)
            mObserved
          , blockTimeStrikeObservedBlockHash = fmap
            (\(Entity _ v)-> blockTimeStrikeObservedJudgementBlockHash v)
            mObserved
          , blockTimeStrikeObservedBlockHeight = fmap
            (\(Entity _ v)-> blockTimeStrikeObservedJudgementBlockHeight v)
            mObserved
          , blockTimeStrikeBlock = blockTimeStrikeBlock strike
          , blockTimeStrikeStrikeMediantime =
            blockTimeStrikeStrikeMediantime strike
          , blockTimeStrikeCreationTime =
            blockTimeStrikeCreationTime strike
          }
        , blockTimeStrikeWithGuessesCountGuessesCount =
          fromIntegral guessesCount
        }

