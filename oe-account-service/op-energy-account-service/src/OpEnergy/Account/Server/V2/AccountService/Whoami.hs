{-- | V2 whoami handler: resolves an account token to the account's stable
 - cross-service identity (personUUID), current display name, and current
 - sandbox wallet balance. Meant for other op-energy services (e.g.
 - oe-offer-service) that need to verify a token they were handed.
 -}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE OverloadedStrings          #-}
module OpEnergy.Account.Server.V2.AccountService.Whoami
  ( whoamiHandler
  , whoami
  ) where

import           Control.Monad.Logger(logError)

import           Database.Persist (Entity(..))

import qualified Data.OpEnergy.Account.API.V1.Account as API
import           Data.OpEnergy.Account.API.V2.WhoAmIResult
                 ( WhoAmIResult(..)
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


-- | V2 whoami endpoint: resolves an account token to identity + balance
whoamiHandler
  :: API.AccountToken
  -> AppM WhoAmIResult
whoamiHandler token =
    let name = "V2.whoamiHandler"
    in profile name $ eitherThrowJSON
      ( runLogging . $(logError))
      $ whoami token

-- | business logic for V2 whoami
whoami
  :: API.AccountToken
  -> AppM (Either CallstackError WhoAmIResult)
whoami token =
    let name = "V2.whoami"
    in profile name $ runExceptPrefixT name $ do
  (Entity _ person) <- exceptTMaybeT accountNotFound
    $ mgetPersonByAccountToken token
  return $! WhoAmIResult
    (apiModelUUIDPerson $ personUuid person)
    (personDisplayName person)
    (personBalance person)
