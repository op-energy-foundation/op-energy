{-- | Everything this service needs from oe-account-service, over HTTP:
 - resolving an AccountToken to an identity (verifyAccountToken), and
 - adjusting that identity's sandbox balance (deductBalance/creditBalance)
 - -- see Data.OpEnergy.Account.Client (op-energy-account-api) for the
 - client machinery this wraps.
 -
 - This is the only place this service talks to another service. Every
 - offer handler that needs to know "whose request is this", or that needs
 - to move sats, goes through here first; nothing in this service reads
 - oe-account-service's DB, decrypts an AccountToken, or stores a balance
 - of its own (see docs/plans/post-offer-api.md's note on why balance
 - moved from a local Wallet table to oe-account-service's own Person
 - table).
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
import qualified Data.OpEnergy.Account.API.V2 as AccountV2
import           Data.OpEnergy.Account.API.V2 (WhoAmIResult)
import qualified Data.OpEnergy.Account.Client as Client

import           OpEnergy.Offer.Server.V1.Class (AppT, State(..))
import           OpEnergy.Offer.Server.V1.Config (configAccountServiceURL, configInternalServiceSharedSecret)
import           OpEnergy.Offer.Server.V1.Metrics (MetricsState(..))
import qualified Prometheus as P

import           OpEnergy.Error
                   ( CallstackError, authenticationFailure, accountServiceUnavailable
                   , insufficientBalance
                   )

-- | (MonadIO m) is all any of these three need -- none of them are
-- Servant-handler-specific (no MonadMonitor use besides P.observeDuration's
-- own IO-level instance, see verifyAccountToken). Kept polymorphic in the
-- underlying monad rather than pinned to AppM (= AppT Handler) so
-- OpEnergy.Offer.Server.V2.Expiry -- which runs in the scheduler loop's
-- AppT IO, not AppT Handler -- can call these too, via
-- OpEnergy.Offer.Server.V1.OfferService.refundAndCloseOffer.

-- | 'accountServiceUnavailable' covers the call to oe-account-service never
-- getting a response at all: an escaped IO exception, or servant-client's
-- own 'ConnectionError' -- oe-account-service down/unreachable, not a
-- verdict on the token. Every other outcome from a completed round trip
-- ('FailureResponse' -- almost always its 401 for an unknown token --
-- 'DecodeFailure', or anything else unexpected) is folded into
-- 'authenticationFailure': from this service's point of view, none of
-- those mean anything other than "this token doesn't check out".
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

-- | Atomically deducts amountSats from personUUID's balance via
-- oe-account-service's internal/balance/deduct. See that route's own
-- description for the atomicity guarantee -- it happens entirely on
-- oe-account-service's side, this is just the call to it.
--
-- Left 'insufficientBalance' means oe-account-service was reached and
-- cleanly refused (400: the account doesn't have enough balance -- or,
-- vanishingly unlikely given the personUUID just came from a successful
-- verifyAccountToken call moments earlier, an unknown personUUID; this
-- service doesn't distinguish the two, both mean "can't deduct"). Left
-- 'accountServiceUnavailable' covers everything else that can go wrong
-- with the call itself (network error, timeout, oe-account-service down,
-- an unexpected response, or this service's own INTERNAL_SERVICE_SHARED_SECRET
-- not matching oe-account-service's).
deductBalance :: (MonadIO m) => AccountAPI.UUID AccountAPI.Person -> Int -> AppT m (Either CallstackError Int)
deductBalance personUUIDV amountSats =
  adjustBalance Client.deductBalance classifyDeduct personUUIDV amountSats
  where
    classifyDeduct :: ClientError -> CallstackError
    classifyDeduct (ConnectionError err) = accountServiceUnavailable (Text.pack (show err))
    classifyDeduct (FailureResponse _ response)
      | statusCode (responseStatusCode response) == 400 = insufficientBalance
    classifyDeduct err = accountServiceUnavailable (Text.pack (show err))

-- | Unconditionally credits amountSats to personUUID's balance via
-- oe-account-service's internal/balance/credit -- used to refund a
-- cancelled/expired offer's stake. See
-- OpEnergy.Offer.Server.V1.OfferService.refundAndCloseOffer for how a
-- failure here (Left) is handled by this service's only two callers: it is
-- treated as best-effort/non-fatal, logged loudly rather than un-doing an
-- already-committed local status change.
creditBalance :: (MonadIO m) => AccountAPI.UUID AccountAPI.Person -> Int -> AppT m (Either CallstackError Int)
creditBalance personUUIDV amountSats =
  -- credit has no "insufficient balance"-style expected failure -- any
  -- non-success response is as much a surprise as a dropped connection.
  adjustBalance Client.creditBalance (accountServiceUnavailable . Text.pack . show) personUUIDV amountSats

-- | shared plumbing for deductBalance/creditBalance: builds the request,
-- makes the call with the configured shared secret, and folds every
-- failure mode (escaped exception, or a completed-but-unsuccessful round
-- trip, via the caller-supplied 'classify') down to a CallstackError.
adjustBalance
  :: (MonadIO m)
  => (Text.Text -> AccountV2.BalanceAdjustRequest -> ClientM AccountV2.BalanceAdjustResult)
  -> (ClientError -> CallstackError)
  -> AccountAPI.UUID AccountAPI.Person
  -> Int
  -> AppT m (Either CallstackError Int)
adjustBalance clientCall classify personUUIDV amountSats = do
  State{ config = config } <- ask
  let url = configAccountServiceURL config
      secret = configInternalServiceSharedSecret config
      request = AccountV2.BalanceAdjustRequest personUUIDV amountSats
  liftIO $! E.handle onConnectException $ do
    eclientResult <- Client.withClientEither url (clientCall secret request)
    return $! either (Left . classify) (Right . AccountV2.balance) eclientResult

onConnectException :: SomeException -> IO (Either CallstackError a)
onConnectException err = return $! Left (accountServiceUnavailable (Text.pack (show err)))
