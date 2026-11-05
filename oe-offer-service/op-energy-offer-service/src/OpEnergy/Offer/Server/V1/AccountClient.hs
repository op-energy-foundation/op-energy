{-- | Everything this service needs from oe-account-service, over HTTP.
 -}
{-# LANGUAGE ScopedTypeVariables #-}
module OpEnergy.Offer.Server.V1.AccountClient
  ( verifyAccountToken
  , deductBalance
  , creditBalance
  , creditWinnings
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
import           Data.OpEnergy.Account.API.V2.LedgerReason
                 ( LedgerReason(..)
                 )
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

-- | takes sats off an account's balance, telling the account service why,
-- so the change shows up in that account's wallet history.
--
-- The answer says which pocket the account service took the stake from:
-- free sats are spent first, and a stake comes out of one pocket, so the
-- caller records what it took and returns the stake to the same pocket
-- when the offer is cancelled or the contract is settled.
deductBalance
  :: (MonadIO m)
  => AccountAPI.UUID AccountAPI.Person
  -> Sats
  -> LedgerReason
  -> Maybe Text.Text
  -> AppT m (Either CallstackError BalanceAdjustResult)
deductBalance personUUIDV amountSats reasonV referenceV =
  adjustBalance Client.deductBalance classifyDeduct personUUIDV amountSats
    Nothing Nothing reasonV referenceV
  where
    classifyDeduct :: ClientError -> CallstackError
    classifyDeduct (ConnectionError err) = accountServiceUnavailable (Text.pack (show err))
    classifyDeduct (FailureResponse _ response)
      | statusCode (responseStatusCode response) == 400 = insufficientBalance
    classifyDeduct err = accountServiceUnavailable (Text.pack (show err))

-- | adds sats to an account's balance, telling the account service why,
-- and how much of the amount is free sats: a stake goes back to the pocket
-- it was paid from, while a profit on top of it is real sats, which can be
-- withdrawn
creditBalance
  :: (MonadIO m)
  => AccountAPI.UUID AccountAPI.Person
  -> Sats
  -> Sats
  -> LedgerReason
  -> Maybe Text.Text
  -> AppT m (Either CallstackError Sats)
creditBalance personUUIDV amountSats freeSatsV reasonV referenceV =
  fmap (fmap balance)
    $ adjustBalance Client.creditBalance
      (accountServiceUnavailable . Text.pack . show)
      personUUIDV amountSats (Just freeSatsV) Nothing reasonV referenceV

-- | pays a settled contract to its winner, naming the account which lost
-- it. The account service counts how many different accounts a winner has
-- won against before it lets winnings be sent out, as a farm of accounts,
-- which stakes the free sats it was given against itself, keeps meeting the
-- same few accounts
creditWinnings
  :: (MonadIO m)
  => AccountAPI.UUID AccountAPI.Person
  -> Sats
  -> Sats
  -> AccountAPI.UUID AccountAPI.Person
  -> Maybe Text.Text
  -> AppT m (Either CallstackError Sats)
creditWinnings personUUIDV amountSats freeSatsV loserUUIDV referenceV =
  fmap (fmap balance)
    $ adjustBalance Client.creditBalance
      (accountServiceUnavailable . Text.pack . show)
      personUUIDV amountSats (Just freeSatsV) (Just loserUUIDV) Winnings
      referenceV

adjustBalance
  :: (MonadIO m)
  => (Text.Text -> BalanceAdjustRequest -> ClientM BalanceAdjustResult)
  -> (ClientError -> CallstackError)
  -> AccountAPI.UUID AccountAPI.Person
  -> Sats
  -> Maybe Sats
  -> Maybe (AccountAPI.UUID AccountAPI.Person)
  -> LedgerReason
  -> Maybe Text.Text
  -> AppT m (Either CallstackError BalanceAdjustResult)
adjustBalance clientCall classify personUUIDV amountSats mfreeSats
    mcounterparty reasonV referenceV = do
  State{ config = config } <- ask
  let url = configAccountServiceURL config
      secret = configInternalServiceSharedSecret config
      request = BalanceAdjustRequest personUUIDV amountSats (Just reasonV)
        referenceV mcounterparty mfreeSats
  liftIO $! E.handle onConnectException $ do
    eclientResult <- Client.withClientEither url (clientCall secret request)
    return $! either (Left . classify) Right eclientResult

onConnectException :: SomeException -> IO (Either CallstackError a)
onConnectException err = return $! Left (accountServiceUnavailable (Text.pack (show err)))
