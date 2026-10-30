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
import           Data.OpEnergy.Account.API.V2.LedgerReason (LedgerReason)
import           Data.OpEnergy.Account.API.V2.BalanceAdjustResult (BalanceAdjustResult(..))
import qualified Data.OpEnergy.Account.Client as Client

import           OpEnergy.Offer.Server.V1.Class (AppT, State(..))
import           OpEnergy.Offer.Server.V1.Config (configAccountServiceURL, configInternalServiceSharedSecret)
import           OpEnergy.Offer.Server.V1.Metrics (MetricsState(..))
import qualified Prometheus as P

import           OpEnergy.Error
                   ( CallstackError, authenticationFailure, accountServiceUnavailable
                   , accountServiceRefused, accountUnknown
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

deductBalance
  :: (MonadIO m)
  => AccountAPI.UUID AccountAPI.Person
  -> Sats
  -> LedgerReason
  -> Maybe Text.Text
  -> AppT m (Either CallstackError Sats)
deductBalance personUUIDV amountSats reasonV referenceV =
  adjustBalance Client.deductBalance classifyDeduct personUUIDV amountSats
    reasonV referenceV
  where
    classifyDeduct :: ClientError -> CallstackError
    classifyDeduct (ConnectionError err) = accountServiceUnavailable (Text.pack (show err))
    classifyDeduct (FailureResponse _ response)
      | statusCode (responseStatusCode response) == 400 = insufficientBalance
    classifyDeduct err = accountServiceUnavailable (Text.pack (show err))

creditBalance
  :: (MonadIO m)
  => AccountAPI.UUID AccountAPI.Person
  -> Sats
  -> LedgerReason
  -> Maybe Text.Text
  -> AppT m (Either CallstackError Sats)
creditBalance personUUIDV amountSats reasonV referenceV =
  adjustBalance Client.creditBalance classifyCredit personUUIDV amountSats
    reasonV referenceV
  where
    -- Only the two answers, which the credit handler gives before it touches
    -- the balance, tell the caller that the credit did not happen: 401 for a
    -- wrong service secret and 400 for an account, which is not there. Every
    -- other failure -- a 500 from around the update, a reply which does not
    -- decode, a lost connection -- leaves it unknown whether the balance was
    -- changed. Only a refused credit can be repeated, so the unknown ones
    -- must not be reported as one
    classifyCredit :: ClientError -> CallstackError
    classifyCredit (ConnectionError err) =
      accountServiceUnavailable (Text.pack (show err))
    -- 400 is the account service answering that it does not have the
    -- account, and 401 that it did not accept this service's secret. Both
    -- credited nothing, and they are told apart as only the second one can
    -- be carried out later: a secret is put right where the services are
    -- configured, while an account, which is not there, stays that way
    classifyCredit err@(FailureResponse _ response)
      | statusCode (responseStatusCode response) == 400 =
        accountUnknown (Text.pack (show err))
      | statusCode (responseStatusCode response) == 401 =
        accountServiceRefused (Text.pack (show err))
    classifyCredit err = accountServiceUnavailable (Text.pack (show err))

adjustBalance
  :: (MonadIO m)
  => (Text.Text -> BalanceAdjustRequest -> ClientM BalanceAdjustResult)
  -> (ClientError -> CallstackError)
  -> AccountAPI.UUID AccountAPI.Person
  -> Sats
  -> LedgerReason
  -- ^ why the balance moves, recorded in the account's ledger
  -> Maybe Text.Text
  -- ^ what it relates to, eg @"offer:42"@, where there is one
  -> AppT m (Either CallstackError Sats)
adjustBalance clientCall classify personUUIDV amountSats reasonV referenceV = do
  State{ config = config } <- ask
  let url = configAccountServiceURL config
      secret = configInternalServiceSharedSecret config
      request = BalanceAdjustRequest personUUIDV amountSats
                  (Just reasonV) referenceV
  liftIO $! E.handle onConnectException $ do
    eclientResult <- Client.withClientEither url (clientCall secret request)
    return $! either (Left . classify) (Right . (\(BalanceAdjustResult b) -> b)) eclientResult

onConnectException :: SomeException -> IO (Either CallstackError a)
onConnectException err = return $! Left (accountServiceUnavailable (Text.pack (show err)))
