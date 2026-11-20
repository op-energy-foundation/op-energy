{-- | A moderator removes somebody from the telegram group.
 -
 - This is a ban in telegram and not on this platform. The two are separate:
 - banning an account here does not remove them from the group, and this
 - does not stop them trading. Nothing links a telegram account to an op
 - energy one unless its owner connected the two themselves, and a moderator
 - may well be banning somebody who never did.
 -
 - It is a ban rather than a kick because that is what telegram does when it
 - is not told for how long: the account cannot come back on its own.
 -}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
module OpEnergy.Offer.Server.V2.TelegramAPI.BanSender
  ( banSenderHandler
  , banSender
  ) where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Logger (logError, logInfo)
import           Control.Monad.Trans (lift)
import           Control.Monad.Trans.Except (ExceptT(..), throwE)
import           Control.Monad.Trans.Reader (ask)
import           Data.Int (Int64)

import           Servant (NoContent(..))

import qualified Data.OpEnergy.Account.API.V1.Account as AccountAPI
import           Data.Text.Show (tshow)

import           OpEnergy.Offer.Server.V1.Class
                 ( AppM, State(..), runLogging, profile)
import           OpEnergy.Offer.Server.V1.Config
                 ( Config(..)
                 , TelegramBotToken(..)
                 )
import qualified OpEnergy.Offer.Server.V1.Telegram.Client as Telegram
import           OpEnergy.Offer.Server.V2.TelegramAPI.CheckModerator
                 ( checkModerator)
import           OpEnergy.Error
                 ( CallstackError, eitherThrowJSON, exceptTMaybeT
                 , runExceptPrefixT, telegramNotConfigured
                 )

-- | POST a ban of one telegram account
banSenderHandler
  :: Int64
  -> AccountAPI.AccountToken
  -> AppM NoContent
banSenderHandler telegramUserId token =
    let name = "V2.TelegramAPI.BanSender.banSenderHandler"
    in profile name $ eitherThrowJSON
      ( runLogging . $(logError))
      $ banSender telegramUserId token

-- | business logic for banning somebody from the group
banSender
  :: Int64
  -> AccountAPI.AccountToken
  -> AppM (Either CallstackError NoContent)
banSender telegramUserId token =
    let name = "V2.TelegramAPI.BanSender.banSender"
    in profile name $ runExceptPrefixT name $ do
  moderatorUUID <- checkModerator token
  State{ telegramManager = manager
       , config = Config
           { configTelegramAPIURL = apiURL
           , configTelegramBotToken = mBotToken
           , configTelegramChatId = mChatId
           }
       } <- lift ask
  botToken <- exceptTMaybeT telegramNotConfigured $! return mBotToken
  chatId' <- exceptTMaybeT telegramNotConfigured $! return mChatId
  response <- ExceptT $ liftIO $ Telegram.withTelegram manager apiURL
    $ Telegram.banChatMember
        (Telegram.BotPath (unTelegramBotToken botToken))
        (Telegram.ChatMemberRequest chatId' telegramUserId)
  -- telegram's own words for the refusal, which say what the bot is not
  -- allowed to do -- usually that it is not an administrator of the group
  -- or may not restrict members
  either throwE (const (return ())) (Telegram.resultOf response)
  -- there is nothing of this in the group's own record afterwards, so the
  -- log line is the only place it is written down
  lift $ runLogging $ $(logInfo)
    ( "banSender: moderator " <> moderatorUUID
    <> " banned telegram account " <> tshow telegramUserId
    <> " from group " <> tshow chatId'
    )
  return NoContent
