{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
module Data.OpEnergy.Account.API.V2.LoginAPI
  ( LoginAPI
  ) where

import           Servant.API

import           Data.OpEnergy.Account.API.V1.Account
                 ( AccountSecret
                 )
import           Data.OpEnergy.Account.API.V2.LoginResult
                 ( LoginResult
                 )

-- | Login API subset: authenticates a person by their account secret.
type LoginAPI
  = ReqBody '[JSON] AccountSecret
    :> Description "Performs login with given account secret. Returns \
                   \LoginResult(token and person UUID) value for being \
                   \used with the rest API calls."
    :> Post '[JSON] LoginResult
