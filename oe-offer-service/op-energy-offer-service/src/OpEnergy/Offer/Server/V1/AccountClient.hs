{-- | Everything this service needs from oe-account-service, over HTTP.
 -}
{-# LANGUAGE ScopedTypeVariables #-}
module OpEnergy.Offer.Server.V1.AccountClient
  ( verifyAccountToken
  , deductBalance
  , creditBalance
  ) where

import           Control.Monad.Trans.Reader (ask)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Exception.Safe (SomeException)
import qualified Control.Exception.Safe as E
import qualified Data.Text as Text

import           Network.HTTP.Types (statusCode)
import           Servant.Client (ClientError(..), ClientM)
import           Servant.Client.Core (responseStatusCode)

import qualified Data.OpEnergy.Account.API.V1.Account as AccountAPI
import qualified Data.OpEnergy.Account.API.V1.UUID as AccountAPI
import           Data.OpEnergy.Account.API.V2.WhoAmIResult (WhoAmIResult)
import           Data.OpEnergy.Account.API.V1.Sats (Sats(..))
import           Data.OpEnergy.Account.API.V2.BalanceAdjustRequest (BalanceAdjustRequest(..))
import           Data.OpEnergy.Account.API.V2.BalanceAdjustResult (BalanceAdjustResult(..))
import qualified Data.OpEnergy.Account.Client as Client

import           OpEnergy.Offer.Server.V1.Class (AppT, State(..))
import           OpEnergy.Offer.Server.V1.Config (configAccountServiceURL, configInternalServiceSharedSecret)
import           OpEnergy.Offer.Server.V1.Metrics (MetricsState(..))
import qualified Prometheus as P

import           OpEnergy.Error
                   ( CallstackError, authenticationFailure, accountServiceUnavailable
                   , insufficientBalance
                   )

verifyAccountToken :: (MonadIO m) => AccountAPI.AccountToken -> AppT m (Either CallstackError WhoAmIResult)
verifyAccountToken token = do
  State{ config = config, metrics = MetricsState{ accountVerifyToken = hist } } <- ask
  let url = configAccountServiceURL config
  liftIO $! P.observeDuration hist $! E.handle onConnectException $ do
    eclientResult <- Client.withClientEither url (Client.getWhoAmI token)
    return $! either classifyWhoAmI Right eclientResult
  where
    classifyWhoAmI :: ClientError -> Either CallstackError WhoAmIResult
    classifyWhoAmI (ConnectionError err) = Left (accountServiceUnavailable (Text.pack (show err)))
    classifyWhoAmI _clientErr = Left authenticationFailure

deductBalance :: (MonadIO m) => AccountAPI.UUID AccountAPI.Person -> Sats -> AppT m (Either CallstackError Sats)
deductBalance personUUIDV amountSats =
  adjustBalance Client.deductBalance classifyDeduct personUUIDV amountSats
  where
    classifyDeduct :: ClientError -> CallstackError
    classifyDeduct (ConnectionError err) = accountServiceUnavailable (Text.pack (show err))
    classifyDeduct (FailureResponse _ response)
      | statusCode (responseStatusCode response) == 400 = insufficientBalance
    classifyDeduct err = accountServiceUnavailable (Text.pack (show err))

creditBalance :: (MonadIO m) => AccountAPI.UUID AccountAPI.Person -> Sats -> AppT m (Either CallstackError Sats)
creditBalance personUUIDV amountSats =
  adjustBalance Client.creditBalance (accountServiceUnavailable . Text.pack . show) personUUIDV amountSats

adjustBalance
  :: (MonadIO m)
  => (Text.Text -> BalanceAdjustRequest -> ClientM BalanceAdjustResult)
  -> (ClientError -> CallstackError)
  -> AccountAPI.UUID AccountAPI.Person
  -> Sats
  -> AppT m (Either CallstackError Sats)
adjustBalance clientCall classify personUUIDV amountSats = do
  State{ config = config } <- ask
  let url = configAccountServiceURL config
      secret = configInternalServiceSharedSecret config
      request = BalanceAdjustRequest personUUIDV amountSats
  liftIO $! E.handle onConnectException $ do
    eclientResult <- Client.withClientEither url (clientCall secret request)
    return $! either (Left . classify) (Right . (\(BalanceAdjustResult b) -> b)) eclientResult

onConnectException :: SomeException -> IO (Either CallstackError a)
onConnectException err = return $! Left (accountServiceUnavailable (Text.pack (show err)))
