{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE EmptyDataDecls           #-}
{-# LANGUAGE OverloadedStrings          #-}
module OpEnergy.BlockTimeStrike.Server.V1.BlockTimeStrikeObserve
  ( observeStrikes
  , withLeastUnobservedConfirmedBlock
  ) where

import           Control.Monad.Trans.Reader (ReaderT, ask, asks)
import           Control.Monad.Logger(NoLoggingT, logDebug, logError)
import           Control.Monad( when, forM_)
import           Control.Monad.Trans.Maybe ( runMaybeT, MaybeT(..))
import           Data.Time.Clock.POSIX( getPOSIXTime)
import           Data.Conduit( (.|) )
import qualified Data.Conduit as C
import qualified Data.Conduit.List as C
import           Control.Monad.Trans
import qualified Data.List as List
import qualified Data.Text as Text

import           Database.Persist
import           Database.Persist.SqlBackend(SqlBackend)
import           Database.Persist.Pagination
import           Prometheus(MonadMonitor)
import           Control.Monad.Trans.Resource(ResourceT)


import           Data.Text.Show (tshow)
import           Data.OpEnergy.Client as Blockspan
import           Data.OpEnergy.API.V1.Block
import           Data.OpEnergy.API.V1.Positive( Positive, fromPositive)
import           Data.OpEnergy.Account.API.V1.BlockTimeStrike
import           OpEnergy.Account.Server.V1.Config (Config(..))
import           OpEnergy.Account.Server.V1.Class ( AppT
                                                  , State(..)
                                                  , profile
                                                  , withDBNOTransactionROUnsafe
                                                  , withDBTransaction
                                                  , runLoggingIO
                                                  , runLogging
                                                  )
import           OpEnergy.BlockTimeStrike.Server.V1.Context(Context, unContext)
import qualified OpEnergy.BlockTimeStrike.Server.V1.Context as Context
import           OpEnergy.BlockTimeStrike.Server.V1.BlockTimeStrikeJudgement
                   ( BlockObserved
                   , BlockMediantimeReachedBlockNotObserved
                   , JudgementBlock
                   )
import qualified OpEnergy.BlockTimeStrike.Server.V1.BlockTimeStrikeJudgement as Judgement
import           OpEnergy.Account.Server.V1.DB.Migrations


-- | search strikes with block height < confirmed block and observe them
observeStrikes
  :: (MonadIO m, MonadMonitor m)
  => Context LeastUnobservedConfirmedTip BlockHeader
  -> AppT m ()
observeStrikes confirmedBlock = profile "observeStrikesByBlockHeight" $ do
  recordsPerReply <- asks (configRecordsPerReply . config)
  state <- ask
  mresultsCount <- withDBTransaction "" $ do
    C.runConduit
      $ queryStrikesThatMaybeObserved recordsPerReply
      .| filterUnobservedStrikes
      .| findJudgementBlock
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
      C.yield $! Judgement.eitherBlockOrMediantimeObservedStrike strikeE (unContext confirmedBlock)
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
          BlockTimeStrikeBlock <=. blockHeaderHeight (unContext confirmedBlock)
        isStrikeMediantimeObserved =
          BlockTimeStrikeStrikeMediantime
          <=. ( fromIntegral $! blockHeaderMediantime (unContext confirmedBlock))
    streamEntities
      (   [isStrikeBlockConfirmed]
      ||. [isStrikeMediantimeObserved]
      )
      BlockTimeStrikeId
      (PageSize (fromPositive recordsPerReply))
      Descend
      (Range Nothing Nothing)
  findJudgementBlock
    :: C.ConduitT
       ( Either (Context BlockObserved (Entity BlockTimeStrike))
                (Context BlockMediantimeReachedBlockNotObserved (Entity BlockTimeStrike))
       )
       ( Either (Context BlockObserved (Entity BlockTimeStrike))
                (Context BlockMediantimeReachedBlockNotObserved (Entity BlockTimeStrike))
       , Context JudgementBlock BlockHeader
       )
       (ReaderT SqlBackend (NoLoggingT (ResourceT IO)))
       ()
  findJudgementBlock = C.awaitForever $ \estrikeC -> do
        C.yield (estrikeC, Context.believeme (unContext confirmedBlock))
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

data LeastUnobservedConfirmedTip
withLeastUnobservedConfirmedBlock
  :: ( MonadIO m
     , MonadMonitor m
     )
  => BlockHeader
  -> ( Context LeastUnobservedConfirmedTip BlockHeader-> AppT m ())
  -> AppT m ()
withLeastUnobservedConfirmedBlock mpersistedBlockHeightC payload = do
  mlatestConfirmedHeight <- runMaybeT $ do
    mret <- MaybeT $ withDBNOTransactionROUnsafe "" $ runMaybeT $ do
      Entity _ record <- MaybeT $ selectFirst [][]
      MaybeT $ return $ blockTimeStrikeDBLatestConfirmedHeight record
    MaybeT $ return mret
  forM_ (orderedListOfUnobservedHeights mlatestConfirmedHeight mpersistedBlockHeightC)
    $ \leastUnobservedConfirmedHeight -> do
      leastUnobservedConfirmedBlock <- getLeastUnobservedConfirmedBlockHeader leastUnobservedConfirmedHeight
      payload leastUnobservedConfirmedBlock
      persistProcessedConfirmedBlock leastUnobservedConfirmedBlock
  where
    orderedListOfUnobservedHeights
      :: Maybe (Context LatestObservedConfirmedTip BlockHeight)
      -> BlockHeader
      -> [ Context LeastUnobservedConfirmedTip BlockHeight]
    orderedListOfUnobservedHeights mcurrent tip =
      List.map Context.believeme  [ nextHeight .. blockHeaderHeight tip]
      where
      nextHeight = case mcurrent of
        Nothing -> 0
        Just some -> (Context.unContext some + 1)


getLeastUnobservedConfirmedBlockHeader
  :: (MonadIO m, MonadMonitor m)
  => (Context LeastUnobservedConfirmedTip BlockHeight)
  -> AppT m (Context LeastUnobservedConfirmedTip BlockHeader)
getLeastUnobservedConfirmedBlockHeader blockHeightC = do
  blockspanURL <- asks (configBlockspanURL . config)
  fmap Context.believeme $! liftIO $ Blockspan.withClient blockspanURL
      $ getBlockByHeight $! unContext blockHeightC

persistProcessedConfirmedBlock
  :: ( MonadIO m
     , MonadMonitor m
     )
  => Context LeastUnobservedConfirmedTip BlockHeader
  -> AppT m (Maybe ())
persistProcessedConfirmedBlock leastUnobservedBlock = do
  meret <- withDBTransaction "" $ do
    mrecord <- selectFirst [][]
    case mrecord of
      Just (Entity recordId _) -> do
        update recordId
          [ BlockTimeStrikeDBLatestConfirmedHeight =. Just (Context.believeme (blockHeaderHeight (unContext leastUnobservedBlock)))
          ]
        return (Right ())
      Nothing -> return (Left "failed to find BlockTimeStrikeDB record, which is not expected, crashing to restart with db migration")
  case meret of
    Just (Right ()) -> return (Just ())
    Nothing -> return Nothing
    Just (Left reason) -> do
      runLogging $ $(logError) reason
      error (Text.unpack reason)

