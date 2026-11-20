{-- | Hands out the one time code, which connects an account's telegram.
 -
 - The code goes into a link to the bot, and pressing Start in telegram
 - sends it back from the telegram account, which pressed it. That is what
 - proves the two accounts are the same person: nothing is typed and no name
 - is trusted.
 -}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
module OpEnergy.Offer.Server.V2.TelegramAPI.StartLink
  ( startLinkHandler
  , startLink
  , unlinkHandler
  , unlink
  ) where

import           Control.Monad (replicateM)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Logger (logError)
import           Control.Monad.Trans (lift)
import           Control.Monad.Trans.Except (throwE)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Time.Clock (addUTCTime)
import           System.Random (getStdRandom, randomR)

import           Servant (NoContent(..))

import qualified Data.OpEnergy.Account.API.V1.Account as AccountAPI
import qualified Data.OpEnergy.Account.API.V1.UUID as AccountAPI
import qualified Data.OpEnergy.Account.API.V2.WhoAmIResult as AccountV2
import           Data.OpEnergy.Offer.API.V1.TelegramMessageInfo
                 ( TelegramLinkStart(..)
                 )
import qualified Data.ByteString.Short as ShortBS
import qualified Data.Text.Encoding as TextEncoding
import           Servant.API (toQueryParam)

import           OpEnergy.Offer.Server.V1.Class
                 ( AppM, runLogging, profile, withDBTransaction)
import qualified OpEnergy.Offer.Server.V1.AccountClient as AccountClient
import           OpEnergy.Offer.Server.V1.TelegramLink
                 ( putLinkCodeTx
                 , unlinkPersonTx
                 )
import           OpEnergy.Offer.Server.V1.Time (getCurrentTimeDB)
import           OpEnergy.Error
                 ( CallstackError, dbQueryError, eitherThrowJSON
                 , runExceptPrefixT
                 )
import           Control.Monad.Trans.Except (ExceptT(..))

-- | how long a code is good for.
--
-- Short, because anybody who gets hold of one before it is spent could
-- connect their own telegram to somebody else's account -- but long enough
-- to open telegram, find the bot and press a button.
linkCodeValidSeconds :: Int
linkCodeValidSeconds = 600

-- | the characters a code is made of: no vowels and no look-alikes, so a
-- code is never an unfortunate word and never misread when somebody types
-- it by hand instead of following the link
codeAlphabet :: String
codeAlphabet = "bcdfghjkmnpqrstvwxyz23456789"

-- | long enough that guessing one inside its ten minutes is hopeless
codeLength :: Int
codeLength = 12

-- | POST a code to connect this account's telegram
startLinkHandler
  :: AccountAPI.AccountToken
  -> AppM TelegramLinkStart
startLinkHandler token =
    let name = "V2.TelegramAPI.StartLink.startLinkHandler"
    in profile name $ eitherThrowJSON
      ( runLogging . $(logError))
      $ startLink token

-- | business logic for handing out a connect code
startLink
  :: AccountAPI.AccountToken
  -> AppM (Either CallstackError TelegramLinkStart)
startLink token =
    let name = "V2.TelegramAPI.StartLink.startLink"
    in profile name $ runExceptPrefixT name $ do
  (personUUID', displayName) <- whoIsCalling token
  code <- liftIO $ fmap Text.pack $ replicateM codeLength
    $ getStdRandom
    $ \generator ->
        let (index, next) = randomR (0, length codeAlphabet - 1) generator
        in (codeAlphabet !! index, next)
  now <- getCurrentTimeDB
  let expiresAt' = addUTCTime (fromIntegral linkCodeValidSeconds) now
  mstored <- lift $ withDBTransaction "V2.TelegramAPI.StartLink"
    $ putLinkCodeTx code personUUID' displayName expiresAt'
  case mstored of
    Nothing -> throwE dbQueryError
    Just () -> return $! TelegramLinkStart
      { linkCode = code
      , expiresInSeconds = fromIntegral linkCodeValidSeconds
      }

-- | POST a disconnect
unlinkHandler
  :: AccountAPI.AccountToken
  -> AppM NoContent
unlinkHandler token =
    let name = "V2.TelegramAPI.StartLink.unlinkHandler"
    in profile name $ eitherThrowJSON
      ( runLogging . $(logError))
      $ unlink token

-- | business logic for disconnecting.
--
-- Connecting was the account owner's choice, so undoing it has to be
-- theirs as well, and it takes nothing but their own token.
unlink
  :: AccountAPI.AccountToken
  -> AppM (Either CallstackError NoContent)
unlink token =
    let name = "V2.TelegramAPI.StartLink.unlink"
    in profile name $ runExceptPrefixT name $ do
  (personUUID', _displayName) <- whoIsCalling token
  mdone <- lift $ withDBTransaction "V2.TelegramAPI.StartLink.unlink"
    $ unlinkPersonTx personUUID'
  case mdone of
    Nothing -> throwE dbQueryError
    Just () -> return NoContent

-- | the caller's account and the name it currently goes by.
--
-- The name is taken here rather than stored once and left, so connecting
-- after a rename stores the new one.
whoIsCalling
  :: AccountAPI.AccountToken
  -> ExceptT CallstackError AppM (Text, Text)
whoIsCalling token = do
  (AccountV2.WhoAmIResult personUUIDV displayNameV _balance _freeSats _role) <-
    ExceptT $ AccountClient.verifyAccountToken token
  return
    ( TextEncoding.decodeUtf8 $! ShortBS.fromShort
        $! AccountAPI.unUUID personUUIDV
    , toQueryParam displayNameV
    )
