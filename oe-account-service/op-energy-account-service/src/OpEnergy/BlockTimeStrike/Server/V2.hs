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

import           OpEnergy.Account.Server.V1.Class ( AppT)

import           Data.OpEnergy.BlockTime.API.V2
import           Data.OpEnergy.BlockTime.API.V2.StrikesAPI
import           Data.OpEnergy.BlockTime.API.V2.GuessesAPI
import qualified OpEnergy.BlockTimeStrike.Server.V2.BlockSpanTimeStrikeService
                 as BlockSpanTimeStrikeService
import qualified OpEnergy.BlockTimeStrike.Server.V2.BlockSpanTimeStrikeGuessService
                 as BlockSpanTimeStrikeGuessService

blockTimeServer :: ServerT BlockTimeV2API (AppT Handler)
blockTimeServer
  =  (BlockSpanTimeStrikeService.handlers :: ServerT StrikesAPI (AppT Handler))

  :<|> ( BlockSpanTimeStrikeGuessService.handlers
         :: ServerT GuessesAPI (AppT Handler)
       )

