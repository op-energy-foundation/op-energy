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
import           Control.Monad.Trans.Except (throwE)
import           Control.Monad (when)
import           Data.Maybe (isJust)

import qualified Data.OpEnergy.Account.API.V1 as V1API
import           Data.OpEnergy.Account.API.V2.RegisterResultV2
                 ( RegisterResultV2(..)
                 )
import           Data.OpEnergy.Account.API.V2.RegisterRequest
                 ( RegisterRequest
                 )
import qualified Data.OpEnergy.Account.API.V2.RegisterRequest
                 as RegisterRequest

import           OpEnergy.Account.Server.V1.Class
                 ( AppM, runLogging, profile)
import qualified OpEnergy.Account.Server.V1.AccountService
                 as V1 ( register
                        , mgetPersonByDisplayName
                        )

import           OpEnergy.Error
                 ( eitherThrowJSON
                 , runExceptPrefixT
                 , CallstackError
                 , displayNameAlreadyTaken
                 )


-- | V2 register endpoint
registerHandler
  :: RegisterRequest
  -> AppM RegisterResultV2
registerHandler req =
    let name = "V2.registerHandler"
    in profile name $ eitherThrowJSON
      ( runLogging . $(logError))
      $ register req

-- | business logic for V2 register. When a display name is
-- requested, checks uniqueness (returning 'displayNameAlreadyTaken'
-- if taken) before passing through to 'V1.register'.
register
  :: RegisterRequest
  -> AppM (Either CallstackError RegisterResultV2)
register req =
    let name = "V2.register"
    in profile name $ runExceptPrefixT name $ do
  -- when caller supplies a name, verify it is available
  case RegisterRequest.displayName req of
    Just requestedName -> do
      mexists <- lift $ V1.mgetPersonByDisplayName requestedName
      when (isJust mexists) $ throwE displayNameAlreadyTaken
    Nothing -> return ()
  result <- lift $ V1.register (RegisterRequest.displayName req)
  return $! RegisterResultV2
    (V1API.accountSecret (result :: V1API.RegisterResult))
    (V1API.accountToken (result :: V1API.RegisterResult))
    (V1API.personUUID (result :: V1API.RegisterResult))
    (V1API.displayName (result :: V1API.RegisterResult))
    False -- newly registered account never has a password
