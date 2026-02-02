{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DuplicateRecordFields      #-}
module Data.OpEnergy.BlockTime.API.V2.StrikeGuessesAPI where

import           Servant.API

import           Data.OpEnergy.BlockTime.API.V2.BlockSpanTimeStrikeGuessesSummary

type StrikeGuessesAPI
  =    "summary"
    :> Description "returns slow and fast guesses count for a given strike"
    :> Get '[JSON] BlockSpanTimeStrikeGuessesSummary



