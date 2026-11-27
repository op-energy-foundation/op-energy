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
  ) where

import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import           Control.Monad.Trans.Except (ExceptT(..))
import           Control.Monad.Logger (logError)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Time.Clock (addUTCTime)
import           System.IO (IOMode(ReadMode), withFile)


import qualified Data.OpEnergy.Account.API.V1.Account as AccountAPI
import           Data.OpEnergy.Offer.API.V1.TelegramLinkStart
                 ( TelegramLinkStart(..)
                 )

import           OpEnergy.Offer.Server.V1.Class
                 ( AppM, runLogging, profile, withDBTransaction)
import           OpEnergy.Offer.Server.V1.TelegramLink (putLinkCodeTx)
import           OpEnergy.Offer.Server.V2.TelegramAPI.CheckModerator
                 ( whoIsCalling
                 )
import           OpEnergy.Offer.Server.V1.Time (getCurrentTimeDB)
import           OpEnergy.Error
                 ( CallstackError, eitherThrowJSON
                 , runExceptPrefixT
                 )

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

-- | a fresh connect code.
--
-- Drawn from the operating system's randomness rather than from
-- System.Random, whose generator is splitmix and is not meant to be
-- unguessable: somebody, who asked for a few codes of their own, could
-- recover its state from them and then predict the code handed to somebody
-- else, press Start with it first, and have that account's chat messages
-- carry their name.
--
-- Bytes of 252 and over are thrown away rather than folded in, as 256 is
-- not a multiple of the alphabet and folding would make four of its
-- letters likelier than the rest.
newLinkCode :: IO Text
newLinkCode = fmap Text.pack (draw codeLength)
  where
    alphabetSize = length codeAlphabet
    highestUnbiased = 256 - (256 `mod` alphabetSize)
    draw 0 = return []
    draw n = do
      bytes <- withFile "/dev/urandom" ReadMode $ \handle ->
        BS.hGet handle (n * 2)
      let picked = [ codeAlphabet !! (fromIntegral b `mod` alphabetSize)
                   | b <- BS.unpack bytes
                   , fromIntegral b < (highestUnbiased :: Int)
                   ]
      rest <- draw (n - length (take n picked))
      return $! take n picked ++ rest

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
  code <- liftIO newLinkCode
  now <- getCurrentTimeDB
  let expiresAt' = addUTCTime (fromIntegral linkCodeValidSeconds) now
  ExceptT $ withDBTransaction "V2.TelegramAPI.StartLink"
    $ putLinkCodeTx code personUUID' displayName expiresAt'
  return $! TelegramLinkStart
    { linkCode = code
    , expiresInSeconds = fromIntegral linkCodeValidSeconds
    }

