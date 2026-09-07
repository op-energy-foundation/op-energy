{-- | V2 displayname/exists handler: checks if a display name is taken.
 - When the name is taken, the response includes 3 available BIP39-style
 - suggestions.
 -}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE OverloadedStrings          #-}
module OpEnergy.Account.Server.V2.AccountService.DisplayNameExists
  ( displayNameExistsHandler
  , displayNameExists
  ) where

import           Control.Monad.Logger(logError)
import           Control.Monad.Trans (lift)
import           Control.Monad       (replicateM)
import           Data.Maybe (isJust)

import qualified Data.OpEnergy.Account.API.V1.Account as API
import           Data.OpEnergy.Account.API.V2.DisplayNameExistsResult
                 ( DisplayNameExistsResult(..)
                 )

import           OpEnergy.Account.Server.V1.Class
                 ( AppM, runLogging, profile)
import           OpEnergy.Account.Server.V1.AccountService
                 ( mgetPersonByDisplayName)
import           OpEnergy.Account.Server.V2.AccountService.SuggestUsername
                 ( generateAvailableBIP39Username)

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

-- | business logic for V2 displayname/exists.  When the name is
-- taken, generates 3 available BIP39-style suggestions.
displayNameExists
  :: API.DisplayName
  -> AppM (Either CallstackError DisplayNameExistsResult)
displayNameExists dn =
    let name = "V2.displayNameExists"
    in profile name $ runExceptPrefixT name $ do
  mperson <- lift $ mgetPersonByDisplayName dn
  let taken = isJust mperson
  suggs <- if taken
    then lift $ replicateM 3 (generateAvailableBIP39Username 5)
    else return []
  return $! DisplayNameExistsResult taken suggs
