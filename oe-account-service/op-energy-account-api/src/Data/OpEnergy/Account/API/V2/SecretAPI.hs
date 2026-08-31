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
    :> Description "Returns the account secret of the account identified \
                   \by the given account token, so that the frontend can \
                   \display the secret link back to its owner. Fails for \
                   \an account registered before secrets were stored \
                   \recoverably, whose secret exists only as a hash: such \
                   \an account has to regenerate to obtain a displayable one."
    :> Get '[JSON] AccountSecretResult

  :<|> "regenerate"
    :> Header'
       '[ Required
        , Strict
        , Description "Account token gotten from /login or /register"
        ]
       "Authorization"
       AccountToken
    :> Description "Replaces the account secret of the account identified \
                   \by the given account token with a freshly generated \
                   \one, and returns it. The previous secret stops working \
                   \immediately, which is the point of the call: it is how \
                   \a person revokes a secret link they have shared or \
                   \lost. The account token is left alone, so the caller's \
                   \own session survives the rotation."
    :> Post '[JSON] AccountSecretResult
