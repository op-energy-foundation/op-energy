{-- | V2 me handler: returns account info for the authenticated user.
 -}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE OverloadedStrings          #-}
module OpEnergy.Account.Server.V2.AccountService.GetMe
  ( getMeHandler
  , getMe
  ) where

import           Control.Monad.Logger(logError)
import           Data.Maybe (isJust)
import           Database.Persist (Entity(..))

import qualified Data.OpEnergy.Account.API.V1.Account as API
import           Data.OpEnergy.Account.API.V2.AccountInfo
                 ( AccountInfo(..)
                 )

import           OpEnergy.Account.Server.V1.Class
                 ( AppM, runLogging, profile)
import           OpEnergy.Account.Server.V1.AccountService
                 ( mgetPersonByAccountToken)
import           OpEnergy.Account.Server.V1.Person

import           OpEnergy.Error
                 ( eitherThrowJSON, runExceptPrefixT
                 , CallstackError, accountNotFound
                 )
import           OpEnergy.ExceptMaybe(exceptTMaybeT)


-- | V2 me endpoint
getMeHandler
  :: API.AccountToken
  -> AppM AccountInfo
getMeHandler token =
    let name = "V2.getMeHandler"
    in profile name $ eitherThrowJSON
      ( runLogging . $(logError))
      $ getMe token

-- | business logic for V2 me
getMe
  :: API.AccountToken
  -> AppM (Either CallstackError AccountInfo)
getMe token =
    let name = "V2.getMe"
    in profile name $ runExceptPrefixT name $ do
  (Entity _ person) <- exceptTMaybeT accountNotFound
    $ mgetPersonByAccountToken token
  return $! AccountInfo
    (personDisplayName person)
    (isJust (personHashedPassword person))
