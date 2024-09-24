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
module OpEnergy.Account.Server.V2
  ( accountServer
  )where

import           Servant

import           Data.OpEnergy.Account.API.V2
import           OpEnergy.Account.Server.V1.Class (AppT)
import           OpEnergy.Account.Server.V2.AccountService

-- | this is the implementation of OpEnergy.Account.API.V1.AccountV1API. Check this type for the reference and API
-- documentation
accountServer :: ServerT AccountV2API (AppT Handler)
accountServer
  = OpEnergy.Account.Server.V2.AccountService.login

