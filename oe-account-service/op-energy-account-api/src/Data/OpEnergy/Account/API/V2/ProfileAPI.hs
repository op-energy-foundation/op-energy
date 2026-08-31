{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
module Data.OpEnergy.Account.API.V2.ProfileAPI
  ( ProfileAPI
  ) where

import           Servant.API

import           Data.OpEnergy.Account.API.V1.Account
                 ( AccountToken
                 , DisplayName
                 )
import           Data.OpEnergy.Account.API.V2.AccountInfo
                 ( AccountInfo
                 )
import           Data.OpEnergy.Account.API.V2.DisplayNameExistsResult
                 ( DisplayNameExistsResult
                 )

-- | Profile API subset: me, displayname, displayname/exists.
type ProfileAPI
  = "me"
    :> Header'
       '[ Required
        , Strict
        , Description "Account token gotten from /login or /register"
        ]
       "Authorization"
       AccountToken
    :> Description "Returns the display name and whether a password is \
                   \set for the account identified by the given token."
    :> Get '[JSON] AccountInfo

  :<|> "displayname"
    :> Header'
       '[ Required
        , Strict
        , Description "Account token gotten from /login or /register"
        ]
       "Authorization"
       AccountToken
    :> ReqBody '[JSON] DisplayName
    :> Description "Renames the account identified by the given token."
    :> Post '[JSON] AccountInfo

  :<|> "displayname"
    :> "exists"
    :> Capture "displayName" DisplayName
    :> Description "Reports whether the given display name is taken."
    :> Get '[JSON] DisplayNameExistsResult
