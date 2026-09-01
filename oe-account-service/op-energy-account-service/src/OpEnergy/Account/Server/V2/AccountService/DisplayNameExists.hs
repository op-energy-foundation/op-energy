{-- | V2 displayname/exists handler: checks if a display name is taken.
 -}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE OverloadedStrings          #-}
module OpEnergy.Account.Server.V2.AccountService.DisplayNameExists
  ( displayNameExistsHandler
  , displayNameExists
  ) where

import           Control.Monad.Logger(logError)
import           Control.Monad.Trans (lift)
import           Data.Maybe (isJust)

import qualified Data.OpEnergy.Account.API.V1.Account as API
import           Data.OpEnergy.Account.API.V2.DisplayNameExistsResult
                 ( DisplayNameExistsResult(..)
                 )

import           OpEnergy.Account.Server.V1.Class
                 ( AppM, runLogging, profile)
import           OpEnergy.Account.Server.V1.AccountService
                 ( mgetPersonByDisplayName)

import           OpEnergy.Error
                 ( eitherThrowJSON, runExceptPrefixT
                 , CallstackError
                 )


-- | V2 displayname/exists endpoint
displayNameExistsHandler
  :: API.DisplayName
  -> AppM DisplayNameExistsResult
displayNameExistsHandler dn =
    let name = "V2.displayNameExistsHandler"
    in profile name $ eitherThrowJSON
      ( runLogging . $(logError))
      $ displayNameExists dn

-- | business logic for V2 displayname/exists
displayNameExists
  :: API.DisplayName
  -> AppM (Either CallstackError DisplayNameExistsResult)
displayNameExists dn =
    let name = "V2.displayNameExists"
    in profile name $ runExceptPrefixT name $ do
  mperson <- lift $ mgetPersonByDisplayName dn
  return $! DisplayNameExistsResult (isJust mperson)
