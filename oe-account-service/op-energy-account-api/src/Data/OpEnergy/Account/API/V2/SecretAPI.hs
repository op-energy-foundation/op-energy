{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
module Data.OpEnergy.Account.API.V2.SecretAPI
  ( SecretAPI
  ) where

import           Servant.API

import           Data.OpEnergy.Account.API.V1.Account
                 ( AccountToken
                 )
import           Data.OpEnergy.Account.API.V2.AccountSecretResult
                 ( AccountSecretResult
                 )

-- | Secret API subset: read back and rotate the account secret.
type SecretAPI
  = Header'
       '[ Required
        , Strict
        , Description "Account token gotten from /login or /register"
        ]
       "Authorization"
       AccountToken
    :> Description "Returns the account secret for the authenticated account. \
                   \Fails if the secret was not stored recoverably."
    :> Get '[JSON] AccountSecretResult

  :<|> "regenerate"
    :> Header'
       '[ Required
        , Strict
        , Description "Account token gotten from /login or /register"
        ]
       "Authorization"
       AccountToken
    :> Description "Generates a new account secret, replacing the previous one. \
                   \The previous secret stops working immediately. \
                   \The account token is unchanged."
    :> Post '[JSON] AccountSecretResult
