{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE OverloadedStrings          #-}
module OpEnergy.BlockTimeStrike.Server.V1.BlockTimeStrikeObserve
  ( observeStrikes
  ) where

import           Control.Monad.Trans.Reader (ReaderT, ask, asks)
import           Control.Monad.Logger(NoLoggingT, logDebug)
import           Control.Monad( when)
import           Data.Time.Clock.POSIX( getPOSIXTime)
import           Data.Conduit( (.|) )
import qualified Data.Conduit as C
import qualified Data.Conduit.List as C
import           Control.Monad.Trans

import           Database.Persist
import           Database.Persist.SqlBackend(SqlBackend)
import           Database.Persist.Pagination
import           Prometheus(MonadMonitor)
import           Control.Monad.Trans.Resource(ResourceT)
import           Servant.Client (BaseUrl)


import           Data.Text.Show (tshow)
import           Data.OpEnergy.Client as Blockspan
import           Data.OpEnergy.API.V1.Block
import           Data.OpEnergy.API.V1.Positive( Positive, fromPositive)
import           Data.OpEnergy.Account.API.V1.BlockTimeStrike
import           OpEnergy.Account.Server.V1.Config (Config(..))
import           OpEnergy.Account.Server.V1.Class (AppT, State(..), profile, withDBTransaction, runLoggingIO )
import           OpEnergy.BlockTimeStrike.Server.V1.Context(Context, unContext)
import qualified OpEnergy.BlockTimeStrike.Server.V1.Context as Context
import           OpEnergy.BlockTimeStrike.Server.V1.BlockTimeStrikeJudgement
                   ( BlockObserved
                   , BlockMediantimeReachedBlockNotObserved
                   , JudgementBlock
                   )
import qualified OpEnergy.BlockTimeStrike.Server.V1.BlockTimeStrikeJudgement as Judgement


-- | search strikes with block height < confirmed block and observe them
observeStrikes :: (MonadIO m, MonadMonitor m) => BlockHeader -> AppT m ()
observeStrikes confirmedBlock = profile "observeStrikesByBlockHeight" $ do
  blockspanURL <- asks (configBlockspanURL . config)
  recordsPerReply <- asks (configRecordsPerReply . config)
  state <- ask
  mresultsCount <- withDBTransaction "" $ do
    C.runConduit
      $ queryStrikesThatMaybeObserved recordsPerReply
      .| filterUnobservedStrikes
      .| findJudgementBlock blockspanURL
      .| calculateResult
      .| insertStrikeResult
      .| calculateResultCount
  case mresultsCount of
    Nothing -> do
      liftIO $ runLoggingIO state $ $(logDebug)
          ("something went wrong during result calculation")
    Just cnt -> do
      liftIO $ runLoggingIO state $ $(logDebug)
          ("calculated results for " <> tshow cnt <> " strikes")
  where
  filterUnobservedStrikes
    :: C.ConduitT
       ( Entity BlockTimeStrike)
       ( Either (Context BlockObserved (Entity BlockTimeStrike))
                (Context BlockMediantimeReachedBlockNotObserved (Entity BlockTimeStrike))
       )
       (ReaderT SqlBackend (NoLoggingT (ResourceT IO)))
       ()
  filterUnobservedStrikes = C.awaitForever $ \strikeE@(Entity strikeId _) -> do -- check if there
                               -- all the data had been discovered already
    let isStrikeOutcomeObserved =
          BlockTimeStrikeObservedStrike ==. strikeId
    isStrikeOutcomeNotObserved <- fmap not $ lift $ exists
      [ isStrikeOutcomeObserved
      ]
    when isStrikeOutcomeNotObserved $ do
      C.yield $! Judgement.eitherBlockOrMediantimeObservedStrike strikeE confirmedBlock
  queryStrikesThatMaybeObserved
    :: Positive Int
    -> C.ConduitT
       ()
       ( Entity BlockTimeStrike)
       (ReaderT SqlBackend (NoLoggingT (ResourceT IO)))
       ()
  queryStrikesThatMaybeObserved recordsPerReply = do
    let
        isStrikeBlockConfirmed =
          BlockTimeStrikeBlock <=. blockHeaderHeight confirmedBlock
        isStrikeMediantimeObserved =
          BlockTimeStrikeStrikeMediantime
          <=. ( fromIntegral $! blockHeaderMediantime confirmedBlock)
    streamEntities
      (   [isStrikeBlockConfirmed]
      ||. [isStrikeMediantimeObserved]
      )
      BlockTimeStrikeId
      (PageSize (fromPositive recordsPerReply))
      Descend
      (Range Nothing Nothing)
  findJudgementBlock
    :: BaseUrl
    -> C.ConduitT
       ( Either (Context BlockObserved (Entity BlockTimeStrike))
                (Context BlockMediantimeReachedBlockNotObserved (Entity BlockTimeStrike))
       )
       ( Either (Context BlockObserved (Entity BlockTimeStrike))
                (Context BlockMediantimeReachedBlockNotObserved (Entity BlockTimeStrike))
       , Context JudgementBlock BlockHeader
       )
       (ReaderT SqlBackend (NoLoggingT (ResourceT IO)))
       ()
  findJudgementBlock blockspanURL = C.awaitForever $ \estrikeC -> do
    case estrikeC of
      Left blockObserved -> do
        judgementBlock <- liftIO $ getJudgementBlockForObservedBlock blockspanURL blockObserved
        C.yield (estrikeC, judgementBlock)
      Right mediantimeObserved -> do
        judgementBlock <- liftIO $ getJudgementBlockForObservedMediantime blockspanURL mediantimeObserved
        C.yield (estrikeC, judgementBlock)
  calculateResult
    :: C.ConduitT
       ( Either (Context BlockObserved (Entity BlockTimeStrike))
                (Context BlockMediantimeReachedBlockNotObserved (Entity BlockTimeStrike))
       , Context JudgementBlock BlockHeader
       )
       ( Either (Context BlockObserved (Entity BlockTimeStrike))
                (Context BlockMediantimeReachedBlockNotObserved (Entity BlockTimeStrike))
       , Context JudgementBlock BlockHeader
       , SlowFast
       )
       (ReaderT SqlBackend (NoLoggingT (ResourceT IO)))
       ()
  calculateResult = C.map (\( estrike, judgementBlock)->
                              ( estrike
                              , judgementBlock
                              , Judgement.judgeStrike estrike judgementBlock
                              )
                          )
  calculateResultCount
    :: C.ConduitT
       (BlockTimeStrikeObservedId)
       C.Void
       (ReaderT SqlBackend (NoLoggingT (ResourceT IO)))
       Int
  calculateResultCount = do
    sumInLoop 0
    where
      sumInLoop acc = do
        mv <- C.await
        case mv of
          Nothing -> return acc
          Just _ -> sumInLoop (acc + 1)
  insertStrikeResult
    :: C.ConduitT
       ( Either (Context BlockObserved (Entity BlockTimeStrike))
                (Context BlockMediantimeReachedBlockNotObserved (Entity BlockTimeStrike))
       , Context JudgementBlock BlockHeader
       , SlowFast
       )
       (BlockTimeStrikeObservedId)
       (ReaderT SqlBackend (NoLoggingT (ResourceT IO)))
       ()
  insertStrikeResult = C.awaitForever $ \( estrikeC
                                         , judgementBlockC
                                         , calculatedResult
                                         ) -> do
    let
        Entity strikeId _ = case estrikeC of
          Left strikeE -> unContext strikeE
          Right strikeE -> unContext strikeE
        judgementBlock = unContext judgementBlockC
    now <- liftIO getPOSIXTime
    key <- lift $ insert $ BlockTimeStrikeObserved
      { blockTimeStrikeObservedStrike = strikeId
      , blockTimeStrikeObservedJudgementBlockHash =
        blockHeaderHash judgementBlock
      , blockTimeStrikeObservedJudgementBlockMediantime =
        fromIntegral (blockHeaderMediantime judgementBlock)
      , blockTimeStrikeObservedCreationTime = now
      , blockTimeStrikeObservedIsFast = calculatedResult
      , blockTimeStrikeObservedJudgementBlockHeight =
        blockHeaderHeight judgementBlock
      }
    C.yield key
  getJudgementBlockForObservedBlock
    :: BaseUrl
    -> Context BlockObserved (Entity BlockTimeStrike)
    -> IO (Context JudgementBlock BlockHeader)
  getJudgementBlockForObservedBlock blockspanURL strikeC = do
    if isWeHaveStrikeBlockHeader
      then return $! Context.believeme confirmedBlock
      else fmap Context.believeme $! Blockspan.withClient blockspanURL
        $ getBlockByHeight (blockTimeStrikeBlock strike)
    where
      Entity _ strike = unContext strikeC
      isWeHaveStrikeBlockHeader = blockHeaderHeight confirmedBlock == blockTimeStrikeBlock strike
  getJudgementBlockForObservedMediantime
    :: BaseUrl
    -> Context BlockMediantimeReachedBlockNotObserved (Entity BlockTimeStrike)
    -> IO (Context JudgementBlock BlockHeader)
  getJudgementBlockForObservedMediantime blockspanURL strikeC = do
    Context.believeme <$> findFirstBlockWithMediantimeNextAfterStrikeMediantimeInLoop confirmedBlock
    where
      Entity _ strike = unContext strikeC
      findFirstBlockWithMediantimeNextAfterStrikeMediantimeInLoop blockWithMediantimeMoreThanStrikeMediantime
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

