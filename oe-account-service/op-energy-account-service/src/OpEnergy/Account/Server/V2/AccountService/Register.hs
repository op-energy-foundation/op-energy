{-- | V2 register handler: creates a new account and returns the full
 - identity including the assigned display name.
 -}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE OverloadedStrings          #-}
module OpEnergy.Account.Server.V2.AccountService.Register
  ( registerHandler
  , register
  ) where

import           Control.Monad.Logger(logError)
import           Control.Monad.Trans (lift)
import           Database.Persist (Entity(..))

import qualified Data.OpEnergy.Account.API.V1 as V1API
import           Data.OpEnergy.Account.API.V2.RegisterResultV2
                 ( RegisterResultV2(..)
                 )

import           OpEnergy.Account.Server.V1.Class
                 ( AppM, runLogging, profile)
import qualified OpEnergy.Account.Server.V1.AccountService
                 as V1 ( register, mgetPersonByAccountToken)
import           OpEnergy.Account.Server.V1.Person

import           OpEnergy.Error
                 ( eitherThrowJSON, runExceptPrefixT
                 , CallstackError, accountNotFound
                 )
import           OpEnergy.ExceptMaybe(exceptTMaybeT)


-- | V2 register endpoint
registerHandler
  :: AppM RegisterResultV2
registerHandler =
    let name = "V2.registerHandler"
    in profile name $ eitherThrowJSON
      ( runLogging . $(logError))
      $ register

-- | business logic for V2 register
register
  :: AppM (Either CallstackError RegisterResultV2)
register =
    let name = "V2.register"
    in profile name $ runExceptPrefixT name $ do
  result <- lift V1.register
  let token = V1API.accountToken (result :: V1API.RegisterResult)
  (Entity _ person) <- exceptTMaybeT accountNotFound
    $ V1.mgetPersonByAccountToken token
  return $! RegisterResultV2
    (V1API.accountSecret (result :: V1API.RegisterResult))
    token
    (V1API.personUUID (result :: V1API.RegisterResult))
    (personDisplayName person)
