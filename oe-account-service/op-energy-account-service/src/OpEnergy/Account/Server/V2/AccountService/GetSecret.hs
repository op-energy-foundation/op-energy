{-- | V2 secret read-back handler: returns the caller's account secret
 - so the frontend can display the secret link back to its owner.
 -}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE OverloadedStrings          #-}
module OpEnergy.Account.Server.V2.AccountService.GetSecret
  ( getSecretHandler
  , getSecret
  ) where

import           Control.Monad.Trans.Reader (ask)
import           Control.Monad.Logger(logError)
import           Control.Monad.Trans (lift)

import           Database.Persist (Entity(..))

import qualified Data.OpEnergy.Account.API.V1.Account as API
import           Data.OpEnergy.Account.API.V2.AccountSecretResult
                 ( AccountSecretResult(..)
                 )

import           OpEnergy.Account.Server.V1.Config
import           OpEnergy.Account.Server.V1.Class
                 ( AppM, State(..), runLogging, profile)
import           OpEnergy.Account.Server.V1.AccountService
                 ( mgetPersonByAccountToken)
import           OpEnergy.Account.Server.V1.Person

import           OpEnergy.Error
                 ( eitherThrowJSON, runExceptPrefixT
                 , CallstackError, accountNotFound, secretNotRecoverable
                 )
import           OpEnergy.ExceptMaybe(exceptTMaybeT)


-- | V2 secret endpoint: reads back the account secret
getSecretHandler
  :: API.AccountToken
  -> AppM AccountSecretResult
getSecretHandler token =
    let name = "V2.getSecretHandler"
    in profile name $ eitherThrowJSON
      ( runLogging . $(logError))
      $ getSecret token

-- | business logic for V2 secret read-back
getSecret
  :: API.AccountToken
  -> AppM (Either CallstackError AccountSecretResult)
getSecret token =
    let name = "V2.getSecret"
    in profile name $ runExceptPrefixT name $ do
  State{ config = Config { configAccountTokenEncryptionPrivateKey =
                             configAccountTokenEncryptionPrivateKey
                         }
       } <- lift ask
  (Entity _ person) <- exceptTMaybeT accountNotFound
    $ mgetPersonByAccountToken token
  encryptedSecret <- exceptTMaybeT secretNotRecoverable
    $! return (personEncryptedSecret person)
  secret <- exceptTMaybeT accountNotFound
    $! return $! decryptSecret configAccountTokenEncryptionPrivateKey encryptedSecret
  return $! AccountSecretResult secret
