{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DuplicateRecordFields      #-}
module Data.OpEnergy.BlockTime.API.V2.StrikeAPI where

import           Servant.API


import           Data.OpEnergy.API.V1.Natural
import           Data.OpEnergy.API.V1.Positive
import qualified Data.OpEnergy.API.V1.Block as BlockV1
import           Data.OpEnergy.BlockTime.API.V2.BlockSpanTimeStrike

type StrikeAPI
  =    Capture "StrikeBlockHeight" BlockV1.BlockHeight
    :> Capture "StrikeMediantime" (Natural Int)
    :> QueryParam'
       '[ Optional
        , Strict
        , Description "defines block span size"
        ]
       "spanSize"
       (Positive Int)
    :> Description "returns blockrate time strike defined by strike block \
                   \height and strike mediantime"
    :> Get '[JSON] BlockSpanTimeStrike

