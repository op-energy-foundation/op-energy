{-- | The routes of telegram's bot API, which the trade chat is mirrored
 - from.
 -
 - Every request carries the bot's token as its first path segment, so a
 - rendered request -- which is what a servant ClientError and an
 - HttpException both carry -- contains the token. Nothing here puts a
 - request, a URL or either of those values into an error or a log line for
 - that reason: a failure is described by its status code, or by telegram's
 - own description of it, and by nothing else.
 -
 - Telegram answers 200 with @ok: false@ for a refusal, so the body says
 - whether a call succeeded and the status code does not. 'resultOf' is what
 - reads that.
 -}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}
module OpEnergy.Offer.Server.V1.Telegram.Client
  ( BotPath(..)
  , TelegramResponse(..)
  , resultOf
  , Update(..)
  , Message(..)
  , TelegramUser(..)
  , TelegramChat(..)
  , ChatMember(..)
  , botUserIdOf
  , senderDisplayNameOf
  , isGroupMessage
  , isPrivateMessage
  , GetUpdatesRequest(..)
  , defaultGetUpdatesRequest
  , ChatMessageRequest(..)
  , ChatMemberRequest(..)
  , SendMessageRequest(..)
  , getUpdates
  , deleteMessage
  , banChatMember
  , getChatMember
  , sendMessage
  , ReactionRequest(..)
  , setMessageReaction
  , MessagePresence(..)
  , messageStillThere
  , newTelegramManager
  , withTelegram
  ) where

import           Data.Aeson
import           Data.Int (Int64)
import           Data.Maybe (fromMaybe)
import           Data.Proxy (Proxy(..))
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Text.Show (tshow)
import           Text.Read (readMaybe)

import qualified Control.Exception.Safe as E
import           Network.HTTP.Client
                 ( Manager, newManager, managerResponseTimeout
                 , responseTimeoutMicro
                 )
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Network.HTTP.Types.Status (statusCode)
import           Servant.API
import           Servant.Client
                 ( BaseUrl, ClientError(..), ClientM, client, mkClientEnv
                 , runClientM
                 )
import           Servant.Client.Core (responseBody, responseStatusCode)

import           OpEnergy.Error (CallstackError, telegramRequestFailed)

-- | the first path segment of every bot API URL: the literal @bot@ followed
-- by the token, as telegram wants them joined.
newtype BotPath = BotPath Text
instance ToHttpApiData BotPath where
  toUrlPiece (BotPath token) = "bot" <> token

-- | what every bot API method answers with.
--
-- @ok@ rather than the status code says whether the call succeeded, and a
-- refusal carries telegram's own description of it, which is safe to repeat:
-- it describes the refusal, not the request.
data TelegramResponse a = TelegramResponse
  { telegramOk :: Bool
  , telegramResult :: Maybe a
  , telegramErrorCode :: Maybe Int
  , telegramDescription :: Maybe Text
  }
  deriving (Show, Eq)
instance FromJSON a => FromJSON (TelegramResponse a) where
  parseJSON = withObject "TelegramResponse" $ \v -> TelegramResponse
    <$> v .: "ok"
    <*> v .:? "result"
    <*> v .:? "error_code"
    <*> v .:? "description"

-- | the result of a call, or the reason telegram gave for refusing it.
--
-- Example:
--
-- > resultOf response -- Left "Bad Request: message can't be deleted"
resultOf :: TelegramResponse a -> Either CallstackError a
resultOf response = case (telegramOk response, telegramResult response) of
  (True, Just result) -> Right result
  _ -> Left $! telegramRequestFailed $! fromMaybe
    ("telegram refused the request" <> code)
    (telegramDescription response)
  where
    code = maybe "" (\c -> " (" <> tshow c <> ")") (telegramErrorCode response)

-- | an account, as telegram describes one.
--
-- @first_name@ is required and everything else is not: most accounts have no
-- username at all, which is why a username is never what a sender is shown
-- as or matched by.
data TelegramUser = TelegramUser
  { userId :: Int64
  , userFirstName :: Text
  , userLastName :: Maybe Text
  , userUsername :: Maybe Text
  }
  deriving (Show, Eq)
instance FromJSON TelegramUser where
  parseJSON = withObject "TelegramUser" $ \v -> TelegramUser
    <$> v .: "id"
    <*> v .: "first_name"
    <*> v .:? "last_name"
    <*> v .:? "username"

-- | a group, a channel or a one to one conversation with the bot. @type@ is
-- what tells them apart, and the mirror cares: a message in the group is
-- chat to be shown, a message in a private conversation is somebody talking
-- to the bot.
data TelegramChat = TelegramChat
  { chatId :: Int64
  , chatType :: Text
  , chatTitle :: Maybe Text
  }
  deriving (Show, Eq)
instance FromJSON TelegramChat where
  parseJSON = withObject "TelegramChat" $ \v -> TelegramChat
    <$> v .: "id"
    <*> v .: "type"
    <*> v .:? "title"

-- | one message. A message sent by an anonymous administrator has no
-- @from@ at all and carries @sender_chat@, the group itself, instead.
data Message = Message
  { messageId :: Int64
  , messageChat :: TelegramChat
  , messageFrom :: Maybe TelegramUser
  , messageSenderChat :: Maybe TelegramChat
  , messageDate :: Int64
  , messageEditDate :: Maybe Int64
  , messageText :: Maybe Text
  }
  deriving (Show, Eq)
instance FromJSON Message where
  parseJSON = withObject "Message" $ \v -> Message
    <$> v .: "message_id"
    <*> v .: "chat"
    <*> v .:? "from"
    <*> v .:? "sender_chat"
    <*> v .: "date"
    <*> v .:? "edit_date"
    <*> v .:? "text"

-- | one update. @message@ and @edited_message@ are both absent from updates
-- this service does not follow, which are ignored rather than refused: the
-- offset still has to move past them.
data Update = Update
  { updateId :: Int64
  , updateMessage :: Maybe Message
  , updateEditedMessage :: Maybe Message
  }
  deriving (Show, Eq)
instance FromJSON Update where
  parseJSON = withObject "Update" $ \v -> Update
    <$> v .: "update_id"
    <*> v .:? "message"
    <*> v .:? "edited_message"

-- | an account's standing in a chat, as @getChatMember@ answers it. Used to
-- find out whether the bot is still an administrator of the group it is
-- supposed to be mirroring.
data ChatMember = ChatMember
  { chatMemberStatus :: Text
  , chatMemberCanDeleteMessages :: Maybe Bool
  , chatMemberCanRestrictMembers :: Maybe Bool
  }
  deriving (Show, Eq)
instance FromJSON ChatMember where
  parseJSON = withObject "ChatMember" $ \v -> ChatMember
    <$> v .: "status"
    <*> v .:? "can_delete_messages"
    <*> v .:? "can_restrict_members"

-- | the bot's own telegram id, which is the part of its token before the
-- colon.
--
-- Taken from the token rather than asked for with getMe, so the membership
-- check costs one call rather than two and needs nothing kept between them.
--
-- Example:
--
-- > botUserIdOf "7123456789:AAE..." -- Just 7123456789
botUserIdOf :: Text -> Maybe Int64
botUserIdOf token =
  readMaybe (Text.unpack (Text.takeWhile (/= ':') token))

-- | the name a message is shown under.
--
-- @first_name@ is the only name telegram always has, so it is what a sender
-- is shown as, with the last name where there is one. A message with no
-- sender was sent by an anonymous administrator, and carries the group
-- instead, so the group's title is what it is shown under.
--
-- Example:
--
-- > senderDisplayNameOf message -- "Naimish V"
senderDisplayNameOf :: Message -> Text
senderDisplayNameOf message = case messageFrom message of
  Just sender -> Text.unwords
    $! filter (not . Text.null)
    $! [ userFirstName sender, fromMaybe "" (userLastName sender) ]
  Nothing -> fromMaybe "anonymous"
    (messageSenderChat message >>= chatTitle)

-- | whether this message was said in a group, which is the only kind the
-- mirror shows. A channel is not one: in a channel only administrators may
-- post, so a channel is not somewhere traders talk to each other.
isGroupMessage :: Message -> Bool
isGroupMessage message =
  chatType (messageChat message) `elem` [ "group", "supergroup" ]

-- | whether this message was said to the bot one to one, which is how an
-- account is connected.
isPrivateMessage :: Message -> Bool
isPrivateMessage message = chatType (messageChat message) == "private"

-- | what a poll asks for.
data GetUpdatesRequest = GetUpdatesRequest
  { getUpdatesOffset :: Maybe Int64
    -- ^ one past the highest update already received. Nothing asks for
    -- whatever telegram still holds, which is at most 24 hours of it
  , getUpdatesLimit :: Int
  , getUpdatesTimeout :: Int
    -- ^ how long telegram holds the poll open while it waits. It answers at
    -- once when there is a message, so this costs nothing and is what makes
    -- the mirror immediate
  , getUpdatesAllowedUpdates :: [Text]
  }
  deriving (Show, Eq)
instance ToJSON GetUpdatesRequest where
  toJSON request = object
    [ "offset" .= getUpdatesOffset request
    , "limit" .= getUpdatesLimit request
    , "timeout" .= getUpdatesTimeout request
    , "allowed_updates" .= getUpdatesAllowedUpdates request
    ]

-- | asks for the messages and the edits of them, and for nothing else.
defaultGetUpdatesRequest :: GetUpdatesRequest
defaultGetUpdatesRequest = GetUpdatesRequest
  { getUpdatesOffset = Nothing
  , getUpdatesLimit = 100
  , getUpdatesTimeout = 25
  , getUpdatesAllowedUpdates = [ "message", "edited_message" ]
  }

-- | names one message of one chat, which is what deleting takes.
data ChatMessageRequest = ChatMessageRequest
  { chatMessageChatId :: Int64
  , chatMessageMessageId :: Int64
  }
  deriving (Show, Eq)
instance ToJSON ChatMessageRequest where
  toJSON request = object
    [ "chat_id" .= chatMessageChatId request
    , "message_id" .= chatMessageMessageId request
    ]

-- | names one account of one chat, which is what banning and asking after a
-- member both take.
data ChatMemberRequest = ChatMemberRequest
  { chatMemberChatId :: Int64
  , chatMemberUserId :: Int64
  }
  deriving (Show, Eq)
instance ToJSON ChatMemberRequest where
  toJSON request = object
    [ "chat_id" .= chatMemberChatId request
    , "user_id" .= chatMemberUserId request
    ]

-- | what the bot says back to somebody, who talked to it. Only ever used
-- to answer a connect attempt: pressing Start and being told nothing is
-- indistinguishable from pressing Start and it not having worked.
data SendMessageRequest = SendMessageRequest
  { sendMessageChatId :: Int64
  , sendMessageText :: Text
  }
  deriving (Show, Eq)
-- | names one message, for the presence probe. The reaction list is always
-- empty and is not a field: this call is never meant to succeed, only to be
-- refused in one of two ways. See 'messageStillThere'.
data ReactionRequest = ReactionRequest
  { reactionChatId :: Int64
  , reactionMessageId :: Int64
  }
  deriving (Show, Eq)
instance ToJSON ReactionRequest where
  toJSON request = object
    [ "chat_id" .= reactionChatId request
    , "message_id" .= reactionMessageId request
    , "reaction" .= ([] :: [Value])
    ]

instance ToJSON SendMessageRequest where
  toJSON request = object
    [ "chat_id" .= sendMessageChatId request
    , "text" .= sendMessageText request
    ]

-- | the methods the mirror uses. Written out one by one rather than built
-- from a shared synonym, so a type error names the method it is in.
type TelegramBotAPI
  = Capture "token" BotPath
    :> "getUpdates"
    :> ReqBody '[JSON] GetUpdatesRequest
    :> Post '[JSON] (TelegramResponse [Update])

  :<|> Capture "token" BotPath
    :> "deleteMessage"
    :> ReqBody '[JSON] ChatMessageRequest
    :> Post '[JSON] (TelegramResponse Bool)

  :<|> Capture "token" BotPath
    :> "banChatMember"
    :> ReqBody '[JSON] ChatMemberRequest
    :> Post '[JSON] (TelegramResponse Bool)

  :<|> Capture "token" BotPath
    :> "getChatMember"
    :> ReqBody '[JSON] ChatMemberRequest
    :> Post '[JSON] (TelegramResponse ChatMember)


  :<|> Capture "token" BotPath
    :> "sendMessage"
    :> ReqBody '[JSON] SendMessageRequest
    :> Post '[JSON] (TelegramResponse Message)

  :<|> Capture "token" BotPath
    :> "setMessageReaction"
    :> ReqBody '[JSON] ReactionRequest
    :> Post '[JSON] (TelegramResponse Bool)

getUpdates :: BotPath -> GetUpdatesRequest -> ClientM (TelegramResponse [Update])
deleteMessage :: BotPath -> ChatMessageRequest -> ClientM (TelegramResponse Bool)
banChatMember :: BotPath -> ChatMemberRequest -> ClientM (TelegramResponse Bool)
getChatMember :: BotPath -> ChatMemberRequest -> ClientM (TelegramResponse ChatMember)
sendMessage :: BotPath -> SendMessageRequest -> ClientM (TelegramResponse Message)
setMessageReaction :: BotPath -> ReactionRequest -> ClientM (TelegramResponse Bool)
getUpdates
  :<|> deleteMessage
  :<|> banChatMember
  :<|> getChatMember
  :<|> sendMessage
  :<|> setMessageReaction =
  client (Proxy :: Proxy TelegramBotAPI)

-- | how long a telegram call may take, given how long a poll asks telegram
-- to hold the request open for.
--
-- Derived from the configured poll timeout rather than fixed, because the
-- two have to stay in that order: http-client gives up after 30 seconds of
-- its own accord, so a deployment, which asked telegram to wait 40, would
-- have every poll read as a failure, back off, and try again for ever,
-- with nothing in the log but telegram being unreachable.
telegramResponseTimeoutMicrosecondsFor :: Int -> Int
telegramResponseTimeoutMicrosecondsFor pollTimeoutSecs =
  (pollTimeoutSecs + 10) * 1000000

-- | the connection manager every telegram call shares.
--
-- Built once at startup and kept in 'OpEnergy.Offer.Server.V1.Class.State'.
-- One per call would mean a fresh TLS handshake every time -- the poll
-- reconnecting continuously, and fifty handshakes per sweep -- and a
-- Manager only releases its idle sockets when it is finalised, so the ones
-- left behind would accumulate against the process's file descriptors.
newTelegramManager :: Int -> IO Manager
newTelegramManager pollTimeoutSecs = newManager tlsManagerSettings
  { managerResponseTimeout = responseTimeoutMicro
      (telegramResponseTimeoutMicrosecondsFor pollTimeoutSecs)
  }

-- | runs one call against telegram.
--
-- Every failure is turned into a reason, which describes it without
-- describing the request: showing the ClientError, or the exception, would
-- print the URL, and the URL contains the bot's token.
--
-- Example:
--
-- > withTelegram apiURL (deleteMessage path request)
withTelegram
  :: Manager
  -> BaseUrl
  -> ClientM a
  -> IO (Either CallstackError a)
withTelegram manager url call = do
  eresult <- E.tryAny $ runClientM call (mkClientEnv manager url)
  return $! case eresult of
    -- deliberately not the exception: it carries the request, and the
    -- request carries the token
    Left _ -> Left $! telegramRequestFailed "could not reach telegram"
    Right (Left clientError) ->
      Left $! telegramRequestFailed (describeClientError clientError)
    Right (Right result) -> Right result

-- | whether one message is still in the group.
data MessagePresence
  = MessageStillThere
  | MessageGone
  | MessagePresenceUnknown Text -- ^ telegram could not be asked, or said
                                -- something this does not recognise. The
                                -- caller must do nothing: a message is only
                                -- ever hidden on a definite answer
  deriving (Show, Eq)

-- | asks telegram whether one message is still in the group.
--
-- Telegram has no deletion update of any kind: a message removed in the
-- telegram app is removed silently, and a bot is never told. So a mirror,
-- which is to match the group, has to ask about messages one by one.
--
-- The call used for it is setMessageReaction with an empty reaction list,
-- which adds nothing and shows nothing to anybody whatever the answer. A
-- message, which is still there, is accepted or is refused REACTION_EMPTY
-- depending on the version telegram is running; a message, which is gone,
-- is refused "message to react not found", because telegram looks the
-- message up before it reads the list.
--
-- Only those two exact refusals are read as an answer. Everything else --
-- an unreachable telegram, a rate limit, "chat not found" when the bot has
-- been removed from the group, an unauthorized token -- is
-- 'MessagePresenceUnknown', and an unknown answer hides nothing.
--
-- That distinction is the whole safety of this: "chat not found" and
-- "message to react not found" both contain the words "not found", and
-- reading the second loosely would mean that a bot, which has been thrown
-- out of its group, answers "gone" for every message it is asked about and
-- the sweep erases the entire feed -- which is the only copy of the chat
-- there will ever be, and which nothing can put back.
messageStillThere
  :: Manager
  -> BaseUrl
  -> BotPath
  -> Int64
  -> Int64
  -> IO MessagePresence
messageStillThere manager url path chatId' messageId' = do
  eresult <- E.tryAny $ runClientM
    (setMessageReaction path (ReactionRequest chatId' messageId'))
    (mkClientEnv manager url)
  return $! case eresult of
    -- deliberately not the exception: it carries the request, and the
    -- request carries the token
    Left _ -> MessagePresenceUnknown "could not reach telegram"
    -- 200 is not the same as accepted: telegram answers 200 with ok false
    -- for a refusal, so the body is what says which, here as everywhere
    -- else in this module
    Right (Right response)
      | telegramOk response -> MessageStillThere
      | otherwise -> presenceOfDescription (telegramDescription response)
    Right (Left clientError) -> presenceOfRefusal clientError

-- | reads one of telegram's refusals of the presence probe.
--
-- The refusal's body is telegram's own JSON and carries no request, so
-- unlike a ClientError it can be read without printing the token.
presenceOfRefusal :: ClientError -> MessagePresence
presenceOfRefusal (FailureResponse _ response) =
  case decode (responseBody response) :: Maybe (TelegramResponse Bool) of
    Nothing -> MessagePresenceUnknown "telegram's refusal could not be read"
    Just body -> presenceOfDescription (telegramDescription body)
presenceOfRefusal clientError =
  MessagePresenceUnknown (describeClientError clientError)

-- | reads telegram's own words for refusing the presence probe.
--
-- Matched in full rather than by the words "not found", which telegram also
-- says when it is the chat it cannot find -- see 'messageStillThere' for
-- what reading that loosely would cost.
presenceOfDescription :: Maybe Text -> MessagePresence
presenceOfDescription Nothing =
  MessagePresenceUnknown "telegram refused without saying why"
presenceOfDescription (Just description)
  | normalised == "BAD REQUEST: MESSAGE TO REACT NOT FOUND" = MessageGone
  | normalised == "BAD REQUEST: MESSAGE_ID_INVALID" = MessageGone
  | "REACTION_EMPTY" `Text.isInfixOf` normalised = MessageStillThere
  | otherwise = MessagePresenceUnknown
      ("telegram refused the probe: " <> description)
  where
    normalised = Text.toUpper (Text.strip description)

-- | describes a client error by its status alone.
--
-- A ClientError carries the request it was made from, whose path contains
-- the bot's token, so neither it nor the request may be shown. What is left
-- -- the status code -- is enough to tell a refusal from an outage.
describeClientError :: ClientError -> Text
describeClientError (FailureResponse _ response) =
  "telegram answered " <> tshow (statusCode (responseStatusCode response))
describeClientError (DecodeFailure _ _) =
  "telegram's answer could not be read"
describeClientError (UnsupportedContentType _ _) =
  "telegram answered in an unexpected format"
describeClientError (InvalidContentTypeHeader _) =
  "telegram answered in an unexpected format"
describeClientError (ConnectionError _) =
  "could not reach telegram"
