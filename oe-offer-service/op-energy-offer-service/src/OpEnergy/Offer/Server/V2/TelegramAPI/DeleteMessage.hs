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
import qualified Data.Text as Text
import           Data.Text.Show (tshow)

import           Data.OpEnergy.Offer.API.V1.TelegramMessageID
                 ( TelegramMessageID(..)
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
                 ( CallstackError, eitherThrowJSON
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
  State{ telegramManager = manager
       , config = Config
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
  response <- ExceptT $ liftIO $ Telegram.withTelegram manager apiURL
    $ Telegram.deleteMessage
        (Telegram.BotPath (unTelegramBotToken botToken))
        (Telegram.ChatMessageRequest chatId' messageId')
  -- telegram answering and saying no about this message is what it does
  -- once a message is about two days old, and that is what the 409 of this
  -- endpoint describes. Everything else it refuses for -- the bot is no
  -- longer an administrator of the group, the group is gone, the request
  -- was rate limited -- is an outage of ours, and answering those as an
  -- old message would file an operational failure under "nothing to be
  -- done about it"
  case Telegram.resultOf response of
    Right _ -> return ()
    Left err
      | tooOldToDelete (Telegram.telegramDescription response) ->
          throwE telegramMessageNotDeletable
      | otherwise -> throwE err
  now <- getCurrentTimeDB
  ExceptT $ withDBTransaction "V2.TelegramAPI.DeleteMessage"
    $ markTelegramMessageDeletedTx chatId' messageId' moderatorUUID now
  -- built from the number rather than from the text, which arrived: every
  -- other producer of a message id spells it that way, and a reader
  -- matching this against the feed would miss a message, which was asked
  -- about under a spelling of its own -- "0004211" for 4211, say
  lift $ publishLiveEvent $! LiveEvent
    { liveEventMessage =
        LiveMessageTelegramDeleted (TelegramMessageID (tshow messageId'))
    , liveEventBalances = []
    }
  return NoContent
  where
    -- telegram says so in words rather than in a code of its own: every
    -- refusal it makes is a 400 with a description, and this is the one,
    -- which means the message is past the window it deletes within
    tooOldToDelete =
      maybe False (Text.isInfixOf "message can't be deleted" . Text.toLower)
