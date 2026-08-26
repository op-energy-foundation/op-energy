{-- | This module exposes the routes of AccountV2API that other op-energy
 - services need: 'GET /api/v2/account/whoami' (verify an AccountToken and
 - learn who it belongs to), and the internal/balance/{deduct,credit} pair
 - (adjust that account's sandbox balance -- the account this service
 - already owns, rather than a caller duplicating a balance column itself).
 -
 - Deliberately not a client for the whole of AccountV2API: a service like
 - oe-offer-service has no business minting logins/passwords/registrations
 - against this one, only verifying a token it already has and adjusting a
 - balance it doesn't store itself. Mirrors Data.OpEnergy.Client (the
 - blockspan-service's own op-energy-api package,
 - ../op-energy-blockspan-service/op-energy-api/src/Data/OpEnergy/Client.hs)
 - exactly -- same 'withClientEither'/'withClient' shape -- since that is
 - this repo's only precedent for one op-energy service calling another.
 -
 - The API types below are not the same Haskell type as
 - 'Data.OpEnergy.Account.API.V2.AccountV2API' -- servant-client only needs
 - the route's shape (path, method, headers, body type) to match the real
 - server, not a shared type alias -- so these only have to be kept in sync
 - by hand with the corresponding branches of that API type.
 -}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE OverloadedStrings     #-}
module Data.OpEnergy.Account.Client
  ( WhoAmIAPI
  , getWhoAmI
  , DeductBalanceAPI
  , deductBalance
  , CreditBalanceAPI
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

import           Data.OpEnergy.Account.API.V1.Account (AccountToken)
import           Data.OpEnergy.Account.API.V2 (WhoAmIResult, BalanceAdjustRequest, BalanceAdjustResult)

type WhoAmIAPI
  = "api" :> "v2" :> "account" :> "whoami"
    :> Header'
       '[ Required
        , Strict
        , Description "Account token gotten from /login or /register"
        ]
       "Authorization"
       AccountToken
    :> Get '[JSON] WhoAmIResult

getWhoAmI :: AccountToken -> ClientM WhoAmIResult
getWhoAmI = client (Proxy :: Proxy WhoAmIAPI)

type DeductBalanceAPI
  = "api" :> "v2" :> "account" :> "internal" :> "balance" :> "deduct"
    :> Header'
       '[ Required
        , Strict
        , Description "shared secret identifying an internal, service-to-service caller"
        ]
       "X-Internal-Service-Secret"
       Text
    :> ReqBody '[JSON] BalanceAdjustRequest
    :> Post '[JSON] BalanceAdjustResult

deductBalance :: Text -> BalanceAdjustRequest -> ClientM BalanceAdjustResult
deductBalance = client (Proxy :: Proxy DeductBalanceAPI)

type CreditBalanceAPI
  = "api" :> "v2" :> "account" :> "internal" :> "balance" :> "credit"
    :> Header'
       '[ Required
        , Strict
        , Description "shared secret identifying an internal, service-to-service caller"
        ]
       "X-Internal-Service-Secret"
       Text
    :> ReqBody '[JSON] BalanceAdjustRequest
    :> Post '[JSON] BalanceAdjustResult

creditBalance :: Text -> BalanceAdjustRequest -> ClientM BalanceAdjustResult
creditBalance = client (Proxy :: Proxy CreditBalanceAPI)

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
