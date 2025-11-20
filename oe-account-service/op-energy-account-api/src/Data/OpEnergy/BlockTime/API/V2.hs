{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DuplicateRecordFields      #-}
module Data.OpEnergy.BlockTime.API.V2 where

import           Servant.API

import           Data.OpEnergy.API.Tags

import qualified Data.OpEnergy.BlockTime.API.V2.StrikesAPI as StrikesAPI
import qualified Data.OpEnergy.BlockTime.API.V2.GuessesAPI as GuessesAPI

type BlockTimeV2API
  = Tags "Strikes API"
    :> "strikes"
    :> StrikesAPI.StrikesAPI

  :<|> Tags "Guesses API"
    :> "strikes"
    :> "guesses"
    :> GuessesAPI.GuessesAPI

