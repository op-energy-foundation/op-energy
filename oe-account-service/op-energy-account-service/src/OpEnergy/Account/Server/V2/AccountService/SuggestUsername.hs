{-- | V2 displayname/suggest handler: generates an available
 - BIP39-style display name.
 -}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE OverloadedStrings          #-}
module OpEnergy.Account.Server.V2.AccountService.SuggestUsername
  ( suggestUsernameHandler
  , suggestUsername
  ) where

import           Control.Monad.Logger(logError)
import           Control.Monad.Trans (lift)

import           Data.OpEnergy.Account.API.V2.SuggestUsernameResult
                 ( SuggestUsernameResult(..)
                 )

import           OpEnergy.Account.Server.V1.Class
                 ( AppM, runLogging, profile)
import           OpEnergy.Account.Server.V1.AccountService
                 ( mgetPersonByDisplayName)
import           OpEnergy.Account.Server.V1.BIP39Words
                 ( generateAvailableBIP39Username)

import           OpEnergy.Error
                 ( eitherThrowJSON
                 , runExceptPrefixT
                 , CallstackError
                 )

-- | V2 displayname/suggest endpoint
suggestUsernameHandler
  :: AppM SuggestUsernameResult
suggestUsernameHandler =
    let name = "V2.suggestUsernameHandler"
    in profile name $ eitherThrowJSON
      ( runLogging . $(logError))
      $ suggestUsername

-- | business logic for V2 displayname/suggest
suggestUsername
  :: AppM (Either CallstackError SuggestUsernameResult)
suggestUsername =
    let name = "V2.suggestUsername"
    in profile name $ runExceptPrefixT name $ do
  dn <- lift $ generateAvailableBIP39Username mgetPersonByDisplayName 5
  return $! SuggestUsernameResult dn
