{-- | The trade chat as the website reads it: a page of the telegram group,
 - newest first.
 -}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
module OpEnergy.Offer.Server.V2.TelegramAPI.GetMessages
  ( getMessagesHandler
  , getMessages
  ) where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Logger (logError)
import           Control.Monad.Trans (lift)
import           Control.Monad.Trans.Reader (ask)
import           Data.Int (Int64)
import qualified Data.Map.Strict as Map
import           Data.Maybe (mapMaybe)
import qualified Data.Text as Text
import           Text.Read (readMaybe)

import           Database.Persist
import           Database.Persist.Postgresql (runSqlPersistMPool)

import           Data.OpEnergy.API.V1.Positive (Positive, fromPositive)
import           Data.OpEnergy.Offer.API.V1.TelegramMessageInfo
                 ( TelegramFeed(..)
                 , TelegramMessageID(..)
                 )

import           OpEnergy.Offer.Server.V1.Class
                 ( AppM, State(..), runLogging, profile)
import           OpEnergy.Offer.Server.V1.Config (Config(..))
import           OpEnergy.Offer.Server.V1.TelegramLink (opEnergyNamesTx)
import           OpEnergy.Offer.Server.V1.TelegramMessage
import           OpEnergy.Error
                 ( CallstackError, eitherThrowJSON, runExceptPrefixT
                 )

-- | how many messages a page holds when the caller does not say
defaultLimit :: Int
defaultLimit = 20

-- | the most a caller may ask for in one page
maxLimit :: Int
maxLimit = 100

-- | GET the trade chat
getMessagesHandler
  :: Maybe TelegramMessageID
  -> Maybe (Positive Int)
  -> AppM TelegramFeed
getMessagesHandler mBefore mLimit =
    let name = "V2.TelegramAPI.GetMessages.getMessagesHandler"
    in profile name $ eitherThrowJSON
      ( runLogging . $(logError))
      $ getMessages mBefore mLimit

-- | business logic for the trade chat
--
-- Answers an empty feed where this deployment mirrors no telegram group,
-- rather than refusing: a website, which shows a chat nobody has configured
-- yet, should show an empty chat and not an error.
getMessages
  :: Maybe TelegramMessageID
  -> Maybe (Positive Int)
  -> AppM (Either CallstackError TelegramFeed)
getMessages mBefore mLimit =
    let name = "V2.TelegramAPI.GetMessages.getMessages"
    in profile name $ runExceptPrefixT name $ do
  State{ offerDBPool = pool
       , config = Config{ configTelegramChatId = mChatId }
       } <- lift ask
  let limit' = maybe defaultLimit (min maxLimit . fromPositive) mLimit
  case mChatId of
    Nothing -> return $! TelegramFeed
      { messages = []
      , limit = fromIntegral limit'
      , hasMore = False
      }
    Just chatId' -> liftIO $ flip runSqlPersistMPool pool $ do
      -- one more than the page is read, so whether there is an older
      -- message is answered by the read, which produced the page rather
      -- than by counting the whole chat
      rows <- selectList
        ( [ TelegramGroupMessageChatId ==. chatId'
          , TelegramGroupMessageDeletedAt ==. Nothing
          ] ++ olderThan mBefore
        )
        [ Desc TelegramGroupMessageMessageId, LimitTo (limit' + 1) ]
      let page = map entityVal (take limit' rows)
      -- read once for the whole page rather than once per message: a page
      -- is twenty messages and they are mostly from the same few people
      names <- opEnergyNamesTx
        $! mapMaybe telegramGroupMessageSenderId page
      return $! TelegramFeed
        { messages = map (nameFor names) page
        , limit = fromIntegral limit'
        , hasMore = length rows > limit'
        }
  where
    -- the op energy name, where this sender connected their telegram.
    -- Most will not have, and a feed row reads properly without it
    nameFor names row = telegramMessageInfoOf
      ( telegramGroupMessageSenderId row >>= flip Map.lookup names )
      row
    -- a cursor, which is not a number, matches nothing rather than
    -- everything: it was not an id this service ever handed out
    olderThan Nothing = []
    olderThan (Just (TelegramMessageID raw)) =
      case readMaybe (Text.unpack raw) :: Maybe Int64 of
        Just messageId' -> [ TelegramGroupMessageMessageId <. messageId' ]
        Nothing -> [ TelegramGroupMessageMessageId <. 0 ]
