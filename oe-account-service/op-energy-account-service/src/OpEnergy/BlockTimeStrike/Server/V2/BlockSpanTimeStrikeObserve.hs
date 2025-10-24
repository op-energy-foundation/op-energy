{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE EmptyDataDecls           #-}
{-# LANGUAGE OverloadedStrings          #-}
module OpEnergy.BlockTimeStrike.Server.V2.BlockSpanTimeStrikeObserve
  ( observeStrikes
  , withLeastUnobservedConfirmedBlock
  ) where

import           Control.Monad.Trans.Reader (ReaderT, ask, asks)
import           Control.Monad.Logger(NoLoggingT, logDebug, logError)
import           Control.Monad( forM_)
import           Control.Monad.Trans.Maybe ( runMaybeT, MaybeT(..))
import           Data.Time.Clock.POSIX( getPOSIXTime)
import           Data.Conduit( (.|) )
import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as C
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
import           OpEnergy.BlockTimeStrike.Server.V1.SlowFast
import           OpEnergy.Account.Server.V1.Config (Config(..))
import           OpEnergy.Account.Server.V1.Class ( AppT
                                                  , State(..)
                                                  , profile
                                                  , withDBTransaction
                                                  , runLoggingIO
                                                  , runLogging
                                                  )
import           OpEnergy.BlockTimeStrike.Server.V1.Context(Context, unContext)
import qualified OpEnergy.BlockTimeStrike.Server.V1.Context as Context
import           OpEnergy.BlockTimeStrike.Server.V2.BlockSpanTimeStrikeJudgement
                   ( BlockObserved
                   , BlockMediantimeReachedBlockNotObserved
                   , JudgementBlock
                   )
import qualified OpEnergy.BlockTimeStrike.Server.V2.BlockSpanTimeStrikeJudgement
                 as Judgement
import qualified OpEnergy.BlockTimeStrike.Server.V2.DBModel as DB
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
      .| C.concatMapM filterUnobservedStrikes
      .| C.map calculateResult
      .| C.mapM insertStrikeResult
      .| calculateResultCount
  case mresultsCount of
    Nothing -> do
      liftIO $ runLoggingIO state $ $(logDebug)
          "something went wrong during result calculation"
    Just cnt -> do
      liftIO $ runLoggingIO state $ $(logDebug)
          ("calculated results for " <> tshow cnt <> " strikes")
  where
  judgementBlockC :: Context JudgementBlock BlockHeader
  judgementBlockC = Context.believeme
    $ unContext
      -- we are sure, that the least unobserved block IS the judgement block
      (confirmedBlock :: (Context LeastUnobservedConfirmedTip BlockHeader))
  queryStrikesThatMaybeObserved
    :: Positive Int
    -> C.ConduitT
       ()
       ( Entity DB.BlockSpanTimeStrike)
       (ReaderT SqlBackend (NoLoggingT (ResourceT IO)))
       ()
  queryStrikesThatMaybeObserved recordsPerReply = do
    let
        isStrikeBlockConfirmed =
          DB.BlockSpanTimeStrikeBlock <=. blockHeaderHeight (unContext confirmedBlock)
        isStrikeMediantimeObserved =
          DB.BlockSpanTimeStrikeMediantime
          <=. ( fromIntegral $! blockHeaderMediantime (unContext confirmedBlock))
    streamEntities
      (   [isStrikeBlockConfirmed]
      ||. [isStrikeMediantimeObserved]
      )
      DB.BlockSpanTimeStrikeId
      (PageSize (fromPositive recordsPerReply))
      Descend
      (Range Nothing Nothing)
  -- | this functions filters out strikes, that have been observed (calculated)
  -- previously, so only unobserved strikes will be passed further down the
  -- stream
  filterUnobservedStrikes
    :: Entity DB.BlockSpanTimeStrike
    -> (ReaderT SqlBackend (NoLoggingT (ResourceT IO)))
       [ Either (Context BlockObserved (Entity DB.BlockSpanTimeStrike))
                (Context BlockMediantimeReachedBlockNotObserved
                  (Entity DB.BlockSpanTimeStrike)
                )
       ]
  filterUnobservedStrikes strikeE@(Entity strikeId _) = do
    let isStrikeOutcomeObserved =
          DB.BlockSpanTimeStrikeObservedStrike ==. strikeId
    isStrikeOutcomeNotObserved <- fmap not $ exists
      [ isStrikeOutcomeObserved ]
    if isStrikeOutcomeNotObserved
      then return
        [ Judgement.eitherBlockOrMediantimeObservedStrike strikeE
          (unContext confirmedBlock)
        ]
      else return []
  calculateResult
    :: Either
       (Context BlockObserved (Entity DB.BlockSpanTimeStrike))
       (Context BlockMediantimeReachedBlockNotObserved (Entity DB.BlockSpanTimeStrike))
    -> ( Either
         (Context BlockObserved (Entity DB.BlockSpanTimeStrike))
         (Context BlockMediantimeReachedBlockNotObserved (Entity DB.BlockSpanTimeStrike))
       , SlowFast
       )
  calculateResult estrike =
    ( estrike
    , Judgement.judgeStrike estrike judgementBlockC
    )
  calculateResultCount
    :: C.ConduitT
       DB.BlockSpanTimeStrikeObservedId
       C.Void
       (ReaderT SqlBackend (NoLoggingT (ResourceT IO)))
       Int
  calculateResultCount = C.length
  insertStrikeResult
    :: ( Either (Context BlockObserved (Entity DB.BlockSpanTimeStrike))
                (Context BlockMediantimeReachedBlockNotObserved
                  (Entity DB.BlockSpanTimeStrike)
                )
       , SlowFast
       )
    -> (ReaderT SqlBackend (NoLoggingT (ResourceT IO)))
       DB.BlockSpanTimeStrikeObservedId
  insertStrikeResult ( estrikeC , calculatedResult) = do
    let
        Entity strikeId _ = case estrikeC of
          Left strikeE -> unContext strikeE
          Right strikeE -> unContext strikeE
        judgementBlock = unContext judgementBlockC
    now <- liftIO getPOSIXTime
    insert $ DB.BlockSpanTimeStrikeObserved
      { DB.blockSpanTimeStrikeObservedStrike = strikeId
      , DB.blockSpanTimeStrikeObservedJudgementBlockHash =
        blockHeaderHash judgementBlock
      , DB.blockSpanTimeStrikeObservedJudgementBlockMediantime =
        fromIntegral (blockHeaderMediantime judgementBlock)
      , DB.blockSpanTimeStrikeObservedCreationTime = now
      , DB.blockSpanTimeStrikeObservedIsFast = calculatedResult
      , DB.blockSpanTimeStrikeObservedJudgementBlockHeight =
        blockHeaderHeight judgementBlock
      }

data LeastUnobservedConfirmedTip
-- | this function gets current latest confirmed tip as an argument
-- and a function, that will receive the least unobserved yet confirmed block,
-- such that this function will ensure, that we will process each new confirmed
-- block in a strict order
withLeastUnobservedConfirmedBlock
  :: ( MonadIO m
     , MonadMonitor m
     )
  => BlockHeader
  -> ( Context LeastUnobservedConfirmedTip BlockHeader-> AppT m ())
  -> AppT m ()
withLeastUnobservedConfirmedBlock mpersistedBlockHeightC payload = do
  mlatestConfirmedHeight <- runMaybeT $ do
    mret <- MaybeT $ withDBTransaction "" $ runMaybeT $ do
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




