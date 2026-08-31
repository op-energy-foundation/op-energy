{-- | V2 set password handler.
 -}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE OverloadedStrings          #-}
module OpEnergy.Account.Server.V2.AccountService.SetPassword
  ( setPasswordHandler
  , setPassword
  ) where

import           Control.Monad.Trans.Reader (ask)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Logger(logError)
import           Control.Monad.Trans (lift)
import qualified Data.Text.Encoding as Text

import           Database.Persist.Postgresql
import qualified Crypto.BCrypt as BCrypt
import           Servant (NoContent(..))

import qualified Data.OpEnergy.Account.API.V1.Account as API
import           Data.OpEnergy.Account.API.V1.Password
                 ( Password(..), HashedPassword(..)
                 )
import           Data.OpEnergy.Account.API.V2.SetPasswordRequest
                 ( SetPasswordRequest(..)
                 )

import           OpEnergy.Account.Server.V1.Class
                 ( AppM, State(..), runLogging, profile)
import           OpEnergy.Account.Server.V1.AccountService
                 ( mgetPersonByAccountToken)
import           OpEnergy.Account.Server.V1.Person

import           OpEnergy.Error
                 ( eitherThrowJSON, runExceptPrefixT
                 , CallstackError, accountNotFound
                 )
import           OpEnergy.ExceptMaybe(exceptTMaybeT)


-- | V2 set password endpoint
setPasswordHandler
  :: API.AccountToken
  -> SetPasswordRequest
  -> AppM NoContent
setPasswordHandler token req =
    let name = "V2.setPasswordHandler"
    in profile name $ eitherThrowJSON
      ( runLogging . $(logError))
      $ setPassword token req

-- | business logic for V2 set password
setPassword
  :: API.AccountToken
  -> SetPasswordRequest
  -> AppM (Either CallstackError NoContent)
setPassword token (SetPasswordRequest (Password rawPw)) =
    let name = "V2.setPassword"
    in profile name $ runExceptPrefixT name $ do
  State{ accountDBPool = pool
       } <- lift ask
  (Entity personKey _) <- exceptTMaybeT accountNotFound
    $ mgetPersonByAccountToken token
  mhashed <- liftIO $! BCrypt.hashPasswordUsingPolicy
    BCrypt.fastBcryptHashingPolicy
    (Text.encodeUtf8 rawPw)
  case mhashed of
    Nothing -> return NoContent -- bcrypt failure; should not happen
    Just hashed -> do
      liftIO $! flip runSqlPersistMPool pool $
        update personKey
          [ PersonHashedPassword =. Just (HashedPassword (Text.decodeUtf8 hashed))
          ]
      return NoContent
