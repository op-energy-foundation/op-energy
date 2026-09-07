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
import           Data.OpEnergy.Account.API.V2.RegisterRequest
                 ( RegisterRequest
                 )

-- | Register API subset: creates a new account.  Accepts an optional
-- display name in the request body; if omitted, the backend generates
-- a BIP39-style name (e.g. @brave_tiger_482@).
type RegisterAPI
  = ReqBody '[JSON] RegisterRequest
    :> Description "Registers a new person and returns their freshly \
                   \generated account secret and account token, along with \
                   \the display name that was assigned to them.  Pass a \
                   \displayName in the JSON body to choose your own; omit \
                   \it (or send {}) to receive a generated BIP39-style name."
    :> Post '[JSON] RegisterResultV2
