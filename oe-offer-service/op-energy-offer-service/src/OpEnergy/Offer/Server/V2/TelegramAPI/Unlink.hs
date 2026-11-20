{-- | Disconnects an account's telegram.
 -
 - Connecting was the account owner's own choice, so undoing it has to be
 - theirs as well, and it takes nothing but their own token.
 -}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
module OpEnergy.Offer.Server.V2.TelegramAPI.Unlink
  ( unlinkHandler
  , unlink
  ) where

import           Control.Monad.Logger (logError)

import           Servant (NoContent(..))

import qualified Data.OpEnergy.Account.API.V1.Account as AccountAPI

import           OpEnergy.Offer.Server.V1.Class
                 ( AppM, runLogging, profile, withDBTransaction)
import           OpEnergy.Offer.Server.V1.TelegramLink (unlinkPersonTx)
import           OpEnergy.Offer.Server.V2.TelegramAPI.CheckModerator
                 ( whoIsCalling
                 )
import           OpEnergy.Error
                 ( CallstackError, dbQueryError, eitherThrowJSON
                 , exceptTMaybeT, runExceptPrefixT
                 )

-- | POST a disconnect
unlinkHandler
  :: AccountAPI.AccountToken
  -> AppM NoContent
unlinkHandler token =
    let name = "V2.TelegramAPI.Unlink.unlinkHandler"
    in profile name $ eitherThrowJSON
      ( runLogging . $(logError))
      $ unlink token

-- | business logic for disconnecting
unlink
  :: AccountAPI.AccountToken
  -> AppM (Either CallstackError NoContent)
unlink token =
    let name = "V2.TelegramAPI.Unlink.unlink"
    in profile name $ runExceptPrefixT name $ do
  (personUUID', _displayName) <- whoIsCalling token
  exceptTMaybeT dbQueryError
    $ withDBTransaction "V2.TelegramAPI.Unlink" $ unlinkPersonTx personUUID'
  return NoContent
