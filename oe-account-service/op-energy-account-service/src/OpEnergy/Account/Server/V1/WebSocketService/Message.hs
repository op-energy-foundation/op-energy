{-- | This module describes WebsocketRequest from frontend and Message from backend
 -}
{-# LANGUAGE ScopedTypeVariables #-}
module OpEnergy.Account.Server.V1.WebSocketService.Message where

import           Data.Aeson as Aeson
import           Data.Text(Text)
import           Network.WebSockets (DataMessage(..), WebSocketsData(..))

import           Data.OpEnergy.API.V1.Block( BlockHeader)

-- | Request from frontend
data WebsocketRequest
  = ActionInit
  | ActionPing
  | ActionWant [Text]
  deriving (Eq, Show)

instance FromJSON WebsocketRequest where
  parseJSON = withObject "WebsocketRequest" $ \v-> do
    action::Text <- v .: "action"
    case action of
      "init" -> return ActionInit
      "ping" -> return ActionPing
      "want" -> ActionWant <$> v .: "data"
      _ -> return ActionPing
instance ToJSON WebsocketRequest where
  toJSON ActionInit = object [ "action" .= ("init" :: Text)]
  toJSON ActionPing = object [ "action" .= ("ping" :: Text)]
  toJSON (ActionWant topics) = object
    [ "action" .= ("want" :: Text)
    , "data" .= topics
    ]
instance WebSocketsData WebsocketRequest where
  fromLazyByteString lbs =
    case Aeson.decode lbs of
      Just ret -> ret
      _ -> error "failed to parse Action from websockets data"
  toLazyByteString action = Aeson.encode action
  fromDataMessage (Text bs _) =
    case Aeson.decode bs of
      Just ret -> ret
      _ -> error "failed to parse Action from websockets data message"
  fromDataMessage (Binary bs) =
    case Aeson.decode bs of
      Just ret -> ret
      _ -> error "failed to parse Action from websockets data message"

data MempoolInfo = MempoolInfo
  { newestConfirmedBlock :: Maybe BlockHeader
  }
  deriving (Show)
instance ToJSON MempoolInfo where
  toJSON mpi = object
    [ "oe-newest-confirmed-block" .= newestConfirmedBlock mpi -- the only field which should be interesting for OpEnergy frontend
    , "mempoolInfo" .= object
      [ "loaded" .= True
      , "size" .= (0:: Int)
      , "bytes" .= (0::Int)
      ]
    , "blocks" .= ([] :: [Text])
    , "mempool-blocks" .= ([] :: [Text])
    , "transactions" .= ([] :: [Text])
    , "backendInfo" .= ("{ \"hostname\": \"test\", \"version\": \"test\", \"gitCommit\": \"test\"}":: Text)
    ]

-- | Message from backend
data Message
  = MessageNewestBlockHeader BlockHeader
  | MessagePong
  deriving (Show)

instance WebSocketsData Message where
  fromLazyByteString lbs =
    case Aeson.decode lbs of
      Just ret -> ret
      _ -> error "failed to parse Action from websockets data"
  toLazyByteString action = Aeson.encode action
  fromDataMessage (Text bs _) =
    case Aeson.decode bs of
      Just ret -> ret
      _ -> error "failed to parse Action from websockets data message"
  fromDataMessage (Binary bs) =
    case Aeson.decode bs of
      Just ret -> ret
      _ -> error "failed to parse Action from websockets data message"
instance FromJSON Message where
  parseJSON = withObject "Message" $ \v-> MessageNewestBlockHeader
    <$> v .: "oe-newest-confirmed-block"
instance ToJSON Message where
  toJSON (MessageNewestBlockHeader header) = object
    [ "oe-newest-confirmed-block" .= toJSON header
    ]
  toJSON (MessagePong) = object
    [ "pong" .= True
    ]
