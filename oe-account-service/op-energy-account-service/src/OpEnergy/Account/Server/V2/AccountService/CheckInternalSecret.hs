{-- | Shared validation for internal service-to-service calls: rejects the
 - call if the caller didn't present the configured shared secret. Uses
 - hash-then-compare to avoid timing side-channels.
 -}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleContexts           #-}
module OpEnergy.Account.Server.V2.AccountService.CheckInternalSecret
  ( checkInternalServiceSecret
  ) where

import           Control.Monad.Trans.Reader (ask)
import           Control.Monad.Trans (lift)
import           Control.Monad.Trans.Except (ExceptT, throwE)
import           Data.Text(Text)
import qualified Data.Text.Encoding as TE

import qualified Crypto.Hash.SHA256 as SHA256

import           OpEnergy.Account.Server.V1.Config (Config(..))
import           OpEnergy.Account.Server.V1.Class (AppT, State(..))

import           OpEnergy.Error (CallstackError, invalidServiceSecret)

-- | validates the X-Internal-Service-Secret header against the configured
-- secret. Uses hash-then-compare to avoid timing side-channels on the raw
-- secret value.
checkInternalServiceSecret
  :: Monad m
  => Text
  -> ExceptT CallstackError (AppT m) ()
checkInternalServiceSecret secret = do
  State{ config = Config{ configInternalServiceSharedSecret = expected } } <- lift ask
  let expectedHash = SHA256.hash (TE.encodeUtf8 expected)
      actualHash   = SHA256.hash (TE.encodeUtf8 secret)
  if expectedHash /= actualHash
    then throwE invalidServiceSecret
    else return ()
