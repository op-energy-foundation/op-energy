{-# LANGUAGE GADTs                      #-}
module OpEnergy.BlockTimeStrike.Server.V2.StrikesAPI
  ( handlers
  ) where

import           Servant

import           Data.OpEnergy.BlockTime.API.V2.StrikesAPI
import           Data.OpEnergy.API.V1.Natural(Natural)
import           Data.OpEnergy.API.V1.Positive

import qualified Data.OpEnergy.Account.API.V1.BlockTimeStrike as API
import qualified Data.OpEnergy.Account.API.V1.FilterRequest as API
import qualified Data.OpEnergy.Account.API.V1.PagingResult as API
import           OpEnergy.Account.Server.V1.Class (AppM, AppT)

import qualified Data.OpEnergy.BlockTime.API.V2.BlockSpanTimeStrike
                 as API
import qualified OpEnergy.BlockTimeStrike.Server.V2.StrikesAPI.GetStrikes
                 as GetStrikes

handlers :: ServerT StrikesAPI (AppT Handler)
handlers
  = ( GetStrikes.getStrikes
          :: Maybe (Positive Int)
          -> Maybe (Natural Int)
          -> Maybe (API.FilterRequest API.BlockTimeStrike API.BlockTimeStrikeFilter)
          -> AppM (API.PagingResult API.BlockSpanTimeStrike)
    )

