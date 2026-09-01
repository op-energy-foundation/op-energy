{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
module Data.OpEnergy.Account.API.V2.RegisterAPI
  ( RegisterAPI
  ) where

import           Servant.API

import           Data.OpEnergy.Account.API.V2.RegisterResultV2
                 ( RegisterResultV2
                 )

-- | Register API subset: creates a new account.
type RegisterAPI
  = Description "Registers a new person and returns their freshly \
               \generated account secret and account token, along with \
               \the display name that was assigned to them."
    :> Post '[JSON] RegisterResultV2
