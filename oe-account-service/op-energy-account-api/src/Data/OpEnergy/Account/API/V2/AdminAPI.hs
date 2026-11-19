{-- | Endpoints only a moderator may call.
 -
 - These are published in the swagger, unlike the internal endpoints: they
 - are called by a browser, by a person holding an account token, and the
 - service refuses the call when that account is not a moderator. Hiding
 - the route would not be what keeps a user out of it; the guard is.
 -}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
module Data.OpEnergy.Account.API.V2.AdminAPI
  ( AdminAPI
  ) where

import           Servant.API

import           Data.OpEnergy.Account.API.V1.Account
                 ( AccountToken, Person
                 )
import           Data.OpEnergy.Account.API.V1.UUID
                 ( UUID
                 )
import           Data.OpEnergy.Account.API.V2.BanUserResult
                 ( BanUserResult
                 )

-- | Admin API subset: what a moderator may do to another account.
type AdminAPI
  = "users"
    :> Capture' '[ Description "UUID of the account to ban" ]
       "personUUID" (UUID Person)
    :> "ban"
    :> Header'
       '[ Required
        , Strict
        , Description "Account token of a moderator"
        ]
       "Authorization"
       AccountToken
    :> Description "Bans the given account. Its token stops working at once, \
                   \it can no longer log in by secret or by password, and \
                   \the offer service cancels its open offers and refunds \
                   \their stakes on its next sweep. Already matched \
                   \contracts are left to settle, as the stake of the other \
                   \side is in them. A ban is permanent. \
                   \Refused with 403 unless the caller is a moderator."
    :> Post '[JSON] BanUserResult
