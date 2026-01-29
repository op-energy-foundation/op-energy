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

module OpEnergy.BlockTimeStrike.Server.V1.DB.Migrations.CalculateBlockTimeStrikeGuessesCount.Migration
  ( migration
  ) where

import           Control.Monad.Logger    ( NoLoggingT )
import           Control.Monad.Trans.Reader (ReaderT)
import           Control.Monad (void)
import           Control.Monad.Trans.Resource(ResourceT)

import           Database.Persist.Postgresql
import           Data.Conduit ((.|), runConduit )
import qualified Data.Conduit.List as C
import           Database.Persist.Pagination

import           Data.OpEnergy.API.V1.Positive(fromPositive)
import           Data.OpEnergy.API.V1.Natural(Natural, verifyNatural)

import           OpEnergy.Account.Server.V1.Config
import           OpEnergy.BlockTimeStrike.Server.V1.DB.Migrations.CalculateBlockTimeStrikeGuessesCount.BlockTimeStrike
import           OpEnergy.BlockTimeStrike.Server.V1.DB.Migrations.CalculateBlockTimeStrikeGuessesCount.BlockTimeStrikeGuess



migration
  :: ( Config -> ReaderT
        SqlBackend
        (Control.Monad.Logger.NoLoggingT
         (ResourceT IO)
        )
        ()
      )
migration config = do
  printMigration mkCalculatedBlockTimeStrikeGuessesCount
  runMigration mkCalculatedBlockTimeStrikeGuessesCount -- new table will be
                      -- introduced and it is safe to run before migration
                      -- the migration itself is a filling of this new table
  runConduit
    $ streamEntities -- search for strikes
      []
      BlockTimeStrikeId
      (PageSize (fromPositive recordsPerReply + 1))
      Descend
      (Range Nothing Nothing)
    .| C.mapM calculateGuessesCount
    .| C.mapM storeGuessesCount
    .| C.sinkNull
  where
    recordsPerReply = configRecordsPerReply config

    calculateGuessesCount
      :: Entity BlockTimeStrike
      -> ReaderT SqlBackend (NoLoggingT (ResourceT IO))
         ( BlockTimeStrikeId
         , Natural Int
         )
    calculateGuessesCount (Entity strikeId _) = do
      guessesCount <- verifyNatural
        <$> count [ BlockTimeStrikeGuessStrike ==. strikeId ] -- count guesses
      return (strikeId, guessesCount)

    storeGuessesCount
      :: ( BlockTimeStrikeId, Natural Int)
      -> ReaderT SqlBackend (NoLoggingT (ResourceT IO))
         ()
    storeGuessesCount (strikeId, guessesCount) = do
      mguessesRecord <- selectFirst
        [ CalculatedBlockTimeStrikeGuessesCountStrike ==. strikeId ]
        []
      case mguessesRecord of
        Nothing -> do -- insert new row for appropriate strike
          void $ insert $ CalculatedBlockTimeStrikeGuessesCount
            { calculatedBlockTimeStrikeGuessesCountGuessesCount = guessesCount
            , calculatedBlockTimeStrikeGuessesCountStrike = strikeId
            }
        Just (Entity guessesRecordId _)-> do -- update existing strike
          update guessesRecordId
            [ CalculatedBlockTimeStrikeGuessesCountGuessesCount =. guessesCount
            ]

