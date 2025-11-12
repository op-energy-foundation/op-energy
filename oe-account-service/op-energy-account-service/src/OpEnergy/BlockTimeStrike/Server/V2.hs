{-- |
 - This module is the top module of backend V1
 -}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE DuplicateRecordFields      #-}
module OpEnergy.BlockTimeStrike.Server.V2
  ( blockTimeServer
  )where

import           Servant



import           Data.OpEnergy.API.V1.Positive
import qualified Data.OpEnergy.Account.API.V1.BlockTimeStrike as API
import qualified Data.OpEnergy.Account.API.V1.BlockTimeStrikeGuess as API
import qualified Data.OpEnergy.Account.API.V1.FilterRequest as API
import qualified Data.OpEnergy.Account.API.V1.PagingResult as API
import           Data.OpEnergy.API.V1.Natural(Natural)
import           OpEnergy.Account.Server.V1.Class (AppM, AppT)

import           Data.OpEnergy.BlockTime.API.V2
import qualified Data.OpEnergy.BlockTime.API.V2.BlockSpanTimeStrike
                 as API
import qualified Data.OpEnergy.BlockTime.API.V2.BlockSpanTimeStrikeGuess
                 as API
import qualified OpEnergy.BlockTimeStrike.Server.V2.BlockSpanTimeStrikeService
                 as BlockSpanTimeStrikeService
import qualified OpEnergy.BlockTimeStrike.Server.V2.BlockSpanTimeStrikeGuessService
                 as BlockSpanTimeStrikeGuessService

blockTimeServer :: ServerT BlockTimeV2API (AppT Handler)
blockTimeServer
  =  (BlockSpanTimeStrikeService.getStrikes
          :: Maybe (Positive Int)
          -> Maybe (Natural Int)
          -> Maybe (API.FilterRequest API.BlockTimeStrike API.BlockTimeStrikeFilter)
          -> AppM (API.PagingResult API.BlockSpanTimeStrike)
       )

  :<|> (BlockSpanTimeStrikeGuessService.getStrikesGuessesHandler
          :: Maybe (Positive Int)
          -> Maybe (Natural Int)
          -> Maybe ( API.FilterRequest
                     API.BlockTimeStrikeGuess
                     API.BlockTimeStrikeGuessFilter
                   )
          -> AppM (API.PagingResult API.BlockSpanTimeStrikeGuess)
       )

