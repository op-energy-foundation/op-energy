{-- | This module exposes the routes of AccountV2API that other op-energy
 - services need: 'GET /api/v2/account/whoami' (verify an AccountToken and
 - learn who it belongs to), and the internal/balance/{deduct,credit} pair
 - (adjust that account's sandbox balance).
 -
 - Endpoint types are imported from their authoritative API modules so
 - changes stay in sync automatically.
 -}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE OverloadedStrings     #-}
module Data.OpEnergy.Account.Client
  ( getWhoAmI
  , deductBalance
  , creditBalance
  , withClientEither
  , withClient
  ) where

import           Data.Proxy               (Proxy(..))
import           Data.Text                (Text)
import           Network.HTTP.Client.TLS as Client
import           Network.HTTP.Client hiding (Proxy)
import           Servant.API
import           Servant.Client hiding ((//), (/:))

import           Data.OpEnergy.Account.API.V1.Account
                 ( AccountToken
                 )
import           Data.OpEnergy.Account.API.V2.WhoAmIResult
                 ( WhoAmIResult
                 )
import           Data.OpEnergy.Account.API.V2.BalanceAdjustRequest
                 ( BalanceAdjustRequest
                 )
import           Data.OpEnergy.Account.API.V2.BalanceAdjustResult
                 ( BalanceAdjustResult
                 )
import           Data.OpEnergy.Account.API.V2.InternalBalanceAPI
                 ( DeductBalanceAPI
                 , CreditBalanceAPI
                 )

-- | full path to the whoami endpoint, used by servant-client
type WhoAmIClientAPI
  = "api" :> "v2" :> "account" :> "whoami"
    :> Header' '[Required, Strict] "Authorization" AccountToken
    :> Get '[JSON] WhoAmIResult

getWhoAmI :: AccountToken -> ClientM WhoAmIResult
getWhoAmI = client (Proxy :: Proxy WhoAmIClientAPI)

-- | full path to the deduct endpoint, reusing the spec from InternalBalanceAPI
type DeductBalanceClientAPI
  = "api" :> "v2" :> "account" :> "internal" :> "balance" :> "deduct"
    :> DeductBalanceAPI

deductBalance :: Text -> BalanceAdjustRequest -> ClientM BalanceAdjustResult
deductBalance = client (Proxy :: Proxy DeductBalanceClientAPI)

-- | full path to the credit endpoint, reusing the spec from InternalBalanceAPI
type CreditBalanceClientAPI
  = "api" :> "v2" :> "account" :> "internal" :> "balance" :> "credit"
    :> CreditBalanceAPI

creditBalance :: Text -> BalanceAdjustRequest -> ClientM BalanceAdjustResult
creditBalance = client (Proxy :: Proxy CreditBalanceClientAPI)

-- | performs one API call against the account service within its own fresh
-- client environment.
-- returns:
--   - Left ClientError - in case of failure
--   - Right result - in case of success
withClientEither :: BaseUrl -> ClientM a -> IO (Either ClientError a)
withClientEither url foo = do
  env <- mkClientEnv <$> newManager tlsManagerSettings <*> pure url
  runClientM foo env

-- | performs one API call against the account service within its own fresh
-- client environment.
-- In case of failure will throw an exception with 'error'
withClient :: BaseUrl -> ClientM a -> IO a
withClient url foo = do
  eresult <- withClientEither url foo
  case eresult of
    Left some -> error $ "withClient: error: " <> show some
    Right some -> return some
