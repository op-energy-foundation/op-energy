{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}
module Data.OpEnergy.Account.API.V2.WhoAmIAPI
  ( WhoAmIAPI
  ) where

import           Servant.API

import           Data.OpEnergy.Account.API.V1.Account
                 ( AccountToken
                 )
import           Data.OpEnergy.Account.API.V2.WhoAmIResult
                 ( WhoAmIResult
                 )

type WhoAmIAPI
  = Header'
    '[ Required
     , Strict
     , Description "Account token gotten from /login or /register"
     ]
    "Authorization"
    AccountToken
  :> Description "Resolves an account token to the account's stable \
                 \cross-service identity (personUUID), current display \
                 \name, and current sandbox wallet balance. Meant for \
                 \other op-energy services (e.g. oe-offer-service) that \
                 \need to verify a token they were handed."
  :> Get '[JSON] WhoAmIResult
