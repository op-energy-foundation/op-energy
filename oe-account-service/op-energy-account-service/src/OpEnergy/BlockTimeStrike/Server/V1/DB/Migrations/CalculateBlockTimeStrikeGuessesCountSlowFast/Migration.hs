{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}


module OpEnergy.BlockTimeStrike.Server.V1.DB.Migrations.CalculateBlockTimeStrikeGuessesCountSlowFast.Migration
  ( migration
  ) where

import           Control.Monad.Logger    ( NoLoggingT )
import           Control.Monad.Trans.Reader (ReaderT)
import           Control.Monad.Trans.Resource(ResourceT)

import           Database.Persist.Postgresql
import           Data.Conduit ((.|), runConduit )
import qualified Data.Conduit.List as C
import           Database.Persist.Pagination

import           Data.OpEnergy.API.V1.Positive(fromPositive)
import           Data.OpEnergy.API.V1.Natural(Natural, verifyNatural)

import           OpEnergy.Account.Server.V1.Config
import           OpEnergy.BlockTimeStrike.Server.V1.SlowFast (SlowFast(..))
import           OpEnergy.BlockTimeStrike.Server.V1.DB.Migrations.CalculateBlockTimeStrikeGuessesCountSlowFast.BlockTimeStrikeGuessBefore
import qualified OpEnergy.BlockTimeStrike.Server.V1.DB.Migrations.CalculateBlockTimeStrikeGuessesCountSlowFast.BlockTimeStrikeGuessAfter
                 as After



migration
  :: ( Config -> ReaderT
        SqlBackend
        (Control.Monad.Logger.NoLoggingT
         (ResourceT IO)
        )
        ()
      )
migration config = do
  -- creates slowCount / fastCount
  printMigration createCalculatedBlockTimeStrikeGuessesCountSlowFastBefore
  runMigration createCalculatedBlockTimeStrikeGuessesCountSlowFastBefore
  runConduit
    $ streamEntities -- search for calculated block time strike guesses
      []
      CalculatedBlockTimeStrikeGuessesCountId
      (PageSize (fromPositive recordsPerReply + 1))
      Descend
      (Range Nothing Nothing)
    .| C.mapM calculateSlowFastCounts
    .| C.mapM updateCalculatedBlockTimeStrikeStrikeGuesses
    .| C.sinkNull
  printMigration After.createCalculatedBlockTimeStrikeGuessesCountSlowFastAfter
  runMigration After.createCalculatedBlockTimeStrikeGuessesCountSlowFastAfter
  where
    recordsPerReply = configRecordsPerReply config
    calculateSlowFastCounts
      :: Entity CalculatedBlockTimeStrikeGuessesCount
      -> ReaderT SqlBackend (NoLoggingT (ResourceT IO))
         ( CalculatedBlockTimeStrikeGuessesCountId
         , Natural Int
         , Natural Int
         )
    calculateSlowFastCounts (Entity guessId guess) = do
      fastCount <- verifyNatural
        <$> count [ BlockTimeStrikeGuessStrike ==.
                    calculatedBlockTimeStrikeGuessesCountStrike guess
                  , BlockTimeStrikeGuessIsFast ==. Fast
                  ] -- count guesses
      slowCount <- verifyNatural
        <$> count [ BlockTimeStrikeGuessStrike ==.
                    ( calculatedBlockTimeStrikeGuessesCountStrike guess)
                  , BlockTimeStrikeGuessIsFast ==. Slow
                  ] -- count guesses
      return (guessId, fastCount, slowCount)
    updateCalculatedBlockTimeStrikeStrikeGuesses
      :: (CalculatedBlockTimeStrikeGuessesCountId
         , Natural Int
         , Natural Int
         )
      -> ReaderT SqlBackend (NoLoggingT (ResourceT IO))
         ()
    updateCalculatedBlockTimeStrikeStrikeGuesses
        (guessId, fastCount, slowCount) =
      update guessId
        [ CalculatedBlockTimeStrikeGuessesCountGuessesCount =. (fastCount + slowCount)
        , CalculatedBlockTimeStrikeGuessesCountFastCount =. Just fastCount
        , CalculatedBlockTimeStrikeGuessesCountSlowCount =. Just slowCount
        ]

