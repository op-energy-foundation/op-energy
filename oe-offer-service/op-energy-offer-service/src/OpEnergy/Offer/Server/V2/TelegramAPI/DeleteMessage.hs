{-- | A moderator removes a message from the trade chat.
 -
 - Telegram is asked first and the copy is marked second. If telegram
 - refuses -- which it does once a message is about two days old -- the copy
 - is left alone and the moderator is told, because the message is still in
 - the group where everybody is reading it. A feed, which hid what the group
 - still shows, would tell the moderator they had dealt with something they
 - had not.
 -}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
module OpEnergy.Offer.Server.V2.TelegramAPI.DeleteMessage
  ( deleteMessageHandler
  , deleteMessage
  ) where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Logger (logError)
import           Control.Monad.Trans (lift)
import           Control.Monad.Trans.Except (ExceptT(..), throwE)
import           Control.Monad.Trans.Reader (ask)

import           Servant (NoContent(..))

import qualified Data.OpEnergy.Account.API.V1.Account as AccountAPI
import           Data.OpEnergy.Offer.API.V1.LiveMessage (LiveMessage(..))
import           Data.OpEnergy.Offer.API.V1.TelegramMessageID
                 ( TelegramMessageID
                 , telegramMessageIDNumber
                 )

import           OpEnergy.Offer.Server.V1.Class
                 ( AppM, State(..), runLogging, profile, withDBTransaction)
import           OpEnergy.Offer.Server.V1.Config
                 ( Config(..)
                 , TelegramBotToken(..)
                 )
import           OpEnergy.Offer.Server.V1.LiveEvent (LiveEvent(..))
import           OpEnergy.Offer.Server.V1.TelegramMessage
                 ( markTelegramMessageDeletedTx
                 )
import qualified OpEnergy.Offer.Server.V1.Telegram.Client as Telegram
import           OpEnergy.Offer.Server.V1.Time (getCurrentTimeDB)
import           OpEnergy.Offer.Server.V1.WebSocketService (publishLiveEvent)
import           OpEnergy.Offer.Server.V2.TelegramAPI.CheckModerator
                 ( checkModerator)
import           OpEnergy.Error
                 ( CallstackError, dbQueryError, eitherThrowJSON
                 , exceptTMaybeT, invalidRequest, runExceptPrefixT
                 , telegramMessageNotDeletable, telegramNotConfigured
                 )

-- | POST a removal of one message
deleteMessageHandler
  :: TelegramMessageID
  -> AccountAPI.AccountToken
  -> AppM NoContent
deleteMessageHandler messageId' token =
    let name = "V2.TelegramAPI.DeleteMessage.deleteMessageHandler"
    in profile name $ eitherThrowJSON
      ( runLogging . $(logError))
      $ deleteMessage messageId' token

-- | business logic for removing a message
deleteMessage
  :: TelegramMessageID
  -> AccountAPI.AccountToken
  -> AppM (Either CallstackError NoContent)
deleteMessage messageIdV token =
    let name = "V2.TelegramAPI.DeleteMessage.deleteMessage"
    in profile name $ runExceptPrefixT name $ do
  moderatorUUID <- checkModerator token
  State{ config = Config
           { configTelegramAPIURL = apiURL
           , configTelegramBotToken = mBotToken
           , configTelegramChatId = mChatId
           }
       } <- lift ask
  botToken <- exceptTMaybeT telegramNotConfigured $! return mBotToken
  chatId' <- exceptTMaybeT telegramNotConfigured $! return mChatId
  -- verified where it arrived, so this never refuses in practice; it is
  -- an exceptTMaybeT rather than a fallback number so that it could never
  -- quietly name a different message than the caller asked about
  messageId' <- exceptTMaybeT (invalidRequest "that is not a message id")
    $! return (telegramMessageIDNumber messageIdV)
  -- telegram first. Only once it has actually removed the message is the
  -- copy marked, so the feed never hides what the group still shows
  -- an unreachable telegram, or one answering with a status, is an outage
  -- rather than a refusal and is reported as one
  response <- ExceptT $ liftIO $ Telegram.withTelegram apiURL
    $ Telegram.deleteMessage
        (Telegram.BotPath (unTelegramBotToken botToken))
        (Telegram.ChatMessageRequest chatId' messageId')
  -- telegram answering and saying no is what it does once a message is
  -- about two days old
  either (const (throwE telegramMessageNotDeletable)) (const (return ()))
    (Telegram.resultOf response)
  now <- getCurrentTimeDB
  exceptTMaybeT dbQueryError $ withDBTransaction "V2.TelegramAPI.DeleteMessage"
    $ markTelegramMessageDeletedTx chatId' messageId' moderatorUUID now
  lift $ publishLiveEvent $! LiveEvent
    { liveEventMessage = LiveMessageTelegramDeleted messageIdV
    , liveEventBalances = []
    }
  return NoContent
