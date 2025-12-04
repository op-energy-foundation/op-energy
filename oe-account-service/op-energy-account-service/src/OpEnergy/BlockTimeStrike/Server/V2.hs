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
import           Data.OpEnergy.BlockTime.API.V2.StrikeAPI
import qualified OpEnergy.BlockTimeStrike.Server.V2.StrikesAPI
                 as StrikesAPI
import qualified OpEnergy.BlockTimeStrike.Server.V2.StrikeAPI
                 as StrikeAPI

blockTimeServer :: ServerT BlockTimeV2API (AppT Handler)
blockTimeServer
  =  (StrikesAPI.handlers :: ServerT StrikesAPI (AppT Handler))

  :<|> ( StrikeAPI.handlers
         :: ServerT StrikeAPI (AppT Handler)
       )

