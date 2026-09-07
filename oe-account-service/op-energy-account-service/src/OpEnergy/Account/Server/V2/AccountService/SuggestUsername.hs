{-- | V2 displayname/suggest handler: generates an available
 - BIP39-style display name.
 -}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE OverloadedStrings          #-}
module OpEnergy.Account.Server.V2.AccountService.SuggestUsername
  ( suggestUsernameHandler
  , suggestUsername
  , generateAvailableBIP39Username
  ) where

import           Control.Monad.Logger(logError)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans (lift)
import           Data.Maybe (isJust)

import qualified Data.OpEnergy.Account.API.V1.Account as API
import           Data.OpEnergy.Account.API.V2.SuggestUsernameResult
                 ( SuggestUsernameResult(..)
                 )

import           OpEnergy.Account.Server.V1.Class
                 ( AppM, runLogging, profile)
import           OpEnergy.Account.Server.V1.AccountService
                 ( mgetPersonByDisplayName)
import           OpEnergy.Account.Server.V1.BIP39Words
                 ( generateBIP39Username)

import           OpEnergy.Error
                 ( eitherThrowJSON, runExceptPrefixT
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
  dn <- lift $ generateAvailableBIP39Username 5
  return $! SuggestUsernameResult dn

-- | generates a BIP39-style display name that is not yet taken,
-- retrying up to @maxRetries@ times on collision.
generateAvailableBIP39Username
  :: Int
  -> AppM API.DisplayName
generateAvailableBIP39Username maxRetries = go maxRetries
  where
    go 0 = do
      -- should be extremely unlikely with 1960^2 * 1000 combinations
      candidate <- liftIO generateBIP39Username
      return candidate -- last attempt, return even if collision
    go n = do
      candidate <- liftIO generateBIP39Username
      mperson <- mgetPersonByDisplayName candidate
      if isJust mperson
        then go (n - 1)
        else return candidate
