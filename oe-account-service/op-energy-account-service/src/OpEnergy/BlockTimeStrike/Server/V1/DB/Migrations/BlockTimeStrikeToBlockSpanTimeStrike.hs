{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE DerivingStrategies         #-}

module OpEnergy.BlockTimeStrike.Server.V1.DB.Migrations.BlockTimeStrikeToBlockSpanTimeStrike
  ( migration
  ) where

import           Control.Monad.Trans.Reader(ReaderT)
import           Control.Monad.Logger    ( NoLoggingT )
import           Control.Monad(void)

import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.Pagination
import           Control.Monad.Trans.Resource(ResourceT)
import           Data.Conduit( (.|) )
import qualified Data.Conduit as C
import qualified Data.Conduit.List as C

import qualified OpEnergy.Account.Server.V1.Config as Config
import qualified OpEnergy.BlockTimeStrike.Server.V1.DB.Migrations.BlockTimeStrikeToBlockSpanTimeStrike.ModelBefore as ModelBefore
import qualified OpEnergy.BlockTimeStrike.Server.V1.DB.Migrations.BlockTimeStrikeToBlockSpanTimeStrike.ModelAfter as ModelAfter
import qualified OpEnergy.PagingResult as PagingResult

migration
  :: Config.Config
  -> ReaderT SqlBackend (NoLoggingT (ResourceT IO)) ()
migration config = do
  forEveryBlockTimeStrike $ \(Entity oldBlockTimeStrikeId oldBlockTimeStrike)-> do
    newBlockSpanStrikeId <- createBlockSpanTimeStrike oldBlockTimeStrike
    forEveryBlockTimeStrikeGuessByStrike oldBlockTimeStrikeId $
      void . createBlockSpanTimeStrikeGuessLike newBlockSpanStrikeId
    forEveryCalculatedBlockTimeStrikeGuessesCount oldBlockTimeStrikeId $
      void . createCalculatedBlockSpanTimeStrikeGuessesCount newBlockSpanStrikeId
    forEveryBlockTimeStrikeObserved oldBlockTimeStrikeId $
      void . createBlockSpanTimeStrikeObserved newBlockSpanStrikeId

  where
  recordsPerReply = Config.configRecordsPerReply config
  forEveryBlockTimeStrike
    :: (  Entity ModelBefore.BlockTimeStrike
       -> ReaderT
          SqlBackend
          (NoLoggingT (ResourceT IO))
          ()
       )
    -> ReaderT
       SqlBackend
       (NoLoggingT (ResourceT IO))
       ()
  forEveryBlockTimeStrike foo = C.runConduit
    $ PagingResult.pagingLoopSource
        Nothing
        recordsPerReply
        []
        Ascend
        ModelBefore.BlockTimeStrikeId
    .| C.mapM foo
    .| C.sinkNull

  forEveryBlockTimeStrikeGuessByStrike
    :: ModelBefore.BlockTimeStrikeId
    -> (  Entity ModelBefore.BlockTimeStrikeGuess
       -> ReaderT
          SqlBackend
          (NoLoggingT (ResourceT IO))
          ()
       )
    -> ReaderT
       SqlBackend
       (NoLoggingT (ResourceT IO))
       ()
  forEveryBlockTimeStrikeGuessByStrike oldBlockTimeStrikeId foo = C.runConduit
    $ PagingResult.pagingLoopSource
        Nothing
        recordsPerReply
        [ ModelBefore.BlockTimeStrikeGuessStrike ==. oldBlockTimeStrikeId ]
        Ascend
        ModelBefore.BlockTimeStrikeGuessId
    .| C.mapM foo
    .| C.sinkNull

  forEveryCalculatedBlockTimeStrikeGuessesCount
    :: ModelBefore.BlockTimeStrikeId
    -> (  Entity ModelBefore.CalculatedBlockTimeStrikeGuessesCount
       -> ReaderT
          SqlBackend
          (NoLoggingT (ResourceT IO))
          ()
       )
    -> ReaderT
       SqlBackend
       (NoLoggingT (ResourceT IO))
       ()
  forEveryCalculatedBlockTimeStrikeGuessesCount oldBlockTimeStrikeId foo =
    C.runConduit
      $ PagingResult.pagingLoopSource
          Nothing
          recordsPerReply
          [ ModelBefore.CalculatedBlockTimeStrikeGuessesCountStrike
            ==. oldBlockTimeStrikeId
          ]
          Ascend
          ModelBefore.CalculatedBlockTimeStrikeGuessesCountId
      .| C.mapM foo
      .| C.sinkNull

  forEveryBlockTimeStrikeObserved
    :: ModelBefore.BlockTimeStrikeId
    -> (  Entity ModelBefore.BlockTimeStrikeObserved
       -> ReaderT
          SqlBackend
          (NoLoggingT (ResourceT IO))
          ()
       )
    -> ReaderT
       SqlBackend
       (NoLoggingT (ResourceT IO))
       ()
  forEveryBlockTimeStrikeObserved oldBlockTimeStrikeId foo = C.runConduit
      $ PagingResult.pagingLoopSource
          Nothing
          recordsPerReply
          [ ModelBefore.BlockTimeStrikeObservedStrike
            ==. oldBlockTimeStrikeId
          ]
          Ascend
          ModelBefore.BlockTimeStrikeObservedId
      .| C.mapM foo
      .| C.sinkNull

  createBlockSpanTimeStrike
    :: ModelBefore.BlockTimeStrike
    -> ReaderT
       SqlBackend
       (NoLoggingT (ResourceT IO))
       ModelAfter.BlockSpanTimeStrikeId
  createBlockSpanTimeStrike oldBlockTimeStrike =
    insert $! ModelAfter.BlockSpanTimeStrike
      { ModelAfter.blockSpanTimeStrikeBlock =
        ModelBefore.blockTimeStrikeBlock oldBlockTimeStrike
      , ModelAfter.blockSpanTimeStrikeSpanSize =
        Config.configDefaultBlockSpanTimeStrikeSpanSize config
      , ModelAfter.blockSpanTimeStrikeMediantime =
        ModelBefore.blockTimeStrikeStrikeMediantime oldBlockTimeStrike
      , ModelAfter.blockSpanTimeStrikeCreationTime =
        ModelBefore.blockTimeStrikeCreationTime oldBlockTimeStrike
      }

  createBlockSpanTimeStrikeGuessLike
    :: ModelAfter.BlockSpanTimeStrikeId
    -> Entity ModelBefore.BlockTimeStrikeGuess
    -> ReaderT
       SqlBackend
       (NoLoggingT (ResourceT IO))
       ModelAfter.BlockSpanTimeStrikeGuessId
  createBlockSpanTimeStrikeGuessLike
      newBlockSpanStrikeId
      (Entity _ oldBlockTimeStrikeGuess) =
    insert $! ModelAfter.BlockSpanTimeStrikeGuess
      { ModelAfter.blockSpanTimeStrikeGuessIsFast =
        ModelBefore.blockTimeStrikeGuessIsFast oldBlockTimeStrikeGuess
      , ModelAfter.blockSpanTimeStrikeGuessCreationTime =
        ModelBefore.blockTimeStrikeGuessCreationTime oldBlockTimeStrikeGuess
      , ModelAfter.blockSpanTimeStrikeGuessStrike = newBlockSpanStrikeId
      , ModelAfter.blockSpanTimeStrikeGuessPerson =
        ModelBefore.blockTimeStrikeGuessPerson oldBlockTimeStrikeGuess
      }

  createCalculatedBlockSpanTimeStrikeGuessesCount
    :: ModelAfter.BlockSpanTimeStrikeId
    -> Entity ModelBefore.CalculatedBlockTimeStrikeGuessesCount
    -> ReaderT
       SqlBackend
       (NoLoggingT (ResourceT IO))
       ModelAfter.CalculatedBlockSpanTimeStrikeGuessesCountId
  createCalculatedBlockSpanTimeStrikeGuessesCount
      newBlockSpanStrikeId
      (Entity _ oldCalculatedBlockTimeStrikeGuessesCount) =
    insert $! ModelAfter.CalculatedBlockSpanTimeStrikeGuessesCount
      { ModelAfter.calculatedBlockSpanTimeStrikeGuessesCountStrike =
        newBlockSpanStrikeId
      , ModelAfter.calculatedBlockSpanTimeStrikeGuessesCountGuessesCount =
        ModelBefore.calculatedBlockTimeStrikeGuessesCountGuessesCount
          oldCalculatedBlockTimeStrikeGuessesCount
      }

  createBlockSpanTimeStrikeObserved
    :: ModelAfter.BlockSpanTimeStrikeId
    -> Entity ModelBefore.BlockTimeStrikeObserved
    -> ReaderT
       SqlBackend
       (NoLoggingT (ResourceT IO))
       ModelAfter.BlockSpanTimeStrikeObservedId
  createBlockSpanTimeStrikeObserved
      newBlockSpanStrikeId
      (Entity _ oldBlockTimeStrikeObserved)=
    insert $! ModelAfter.BlockSpanTimeStrikeObserved
      { ModelAfter.blockSpanTimeStrikeObservedJudgementBlockMediantime =
        ModelBefore.blockTimeStrikeObservedJudgementBlockMediantime oldBlockTimeStrikeObserved
      , ModelAfter.blockSpanTimeStrikeObservedJudgementBlockHash =
        ModelBefore.blockTimeStrikeObservedJudgementBlockHash oldBlockTimeStrikeObserved
      , ModelAfter.blockSpanTimeStrikeObservedJudgementBlockHeight =
        ModelBefore.blockTimeStrikeObservedJudgementBlockHeight oldBlockTimeStrikeObserved
      , ModelAfter.blockSpanTimeStrikeObservedIsFast =
        ModelBefore.blockTimeStrikeObservedIsFast oldBlockTimeStrikeObserved
      , ModelAfter.blockSpanTimeStrikeObservedCreationTime =
        ModelBefore.blockTimeStrikeObservedCreationTime oldBlockTimeStrikeObserved
      , ModelAfter.blockSpanTimeStrikeObservedStrike = newBlockSpanStrikeId
      }

  -- C.runConduit
  --   $ streamEntities
  --     [ isStrikeOutcomeAlreadyCalculated ]
  --     BlockTimeStrikeId
  --     (PageSize (fromPositive recordsPerReply))
  --     Ascend
  --     (Range Nothing Nothing)
  --   .| ( C.awaitForever $ \(Entity strikeId strike) -> do
  --        case ( blockTimeStrikeObservedResult1 strike
  --             , do -- ensure that either both fields exist or none
  --               hash <- blockTimeStrikeObservedBlockHash1 strike
  --               mediantime <- blockTimeStrikeObservedBlockMediantime1 strike
  --               return (hash, mediantime)
  --             ) of
  --          (Just result, Just (judgementBlockHash, judgementBlockMediantime)) -> do -- any result exist
  --            -- move result
  --            now <- liftIO getPOSIXTime
  --            void $ lift $ insert $! BlockTimeStrikeObserved
  --              { blockTimeStrikeObservedIsFast = slowFastToBool result
  --              , blockTimeStrikeObservedCreationTime = floor now
  --              , blockTimeStrikeObservedStrike = strikeId
  --              , blockTimeStrikeObservedJudgementBlockMediantime =
  --                judgementBlockMediantime
  --              , blockTimeStrikeObservedJudgementBlockHash =
  --                judgementBlockHash
  --              , blockTimeStrikeObservedJudgementBlockHeight =
  --                blockTimeStrikeBlock strike
  --              }
  --            return ()
  --          _ -> return () -- the rest will be recalculated by the main outcome calculation routine
  --      )
  --   .| C.sinkNull
  -- -- 3. remove old columns
  -- rawExecute (Text.unlines $
  --             [ "ALTER TABLE \"block_time_strike\" DROP COLUMN \"observed_result\";"
  --             , "ALTER TABLE \"block_time_strike\" DROP COLUMN \"observed_block_mediantime\";"
  --             , "ALTER TABLE \"block_time_strike\" DROP COLUMN \"observed_block_hash\";"
  --             ]
  --            ) []
  --
