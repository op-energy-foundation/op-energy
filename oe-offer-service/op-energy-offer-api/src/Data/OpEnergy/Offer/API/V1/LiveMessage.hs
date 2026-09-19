{-- | Messages of the offer service's websocket: requests from the frontend
 - and live notifications about offers, contracts and new blocks.
 -
 - Notifications about offers and contracts only say what has changed.
 - The frontend is expected to reload the affected data with the offer API.
 - A new block's notification carries the chain tip's height and
 - mediantime.
 -
 - Notifications sent to every connection are numbered 1, 2, ... from the
 - start of the service. Every connection first receives 'LiveMessageHello'
 - with the number of the last notification sent before it opened, and
 - afterwards every later notification in order, so a gap in the numbers
 - means that one was missed. Messages to one connection only have no
 - number. The numbers start again when the service restarts, so they can
 - only be compared within one connection: after a reconnect, the frontend
 - has to reload its data.
 -}
{-# LANGUAGE OverloadedStrings          #-}
module Data.OpEnergy.Offer.API.V1.LiveMessage
  ( LiveRequest(..)
  , LiveMessage(..)
  , SequencedMessage(..)
  ) where

import           Data.Aeson
import           Data.Aeson.Types           (Pair)
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import           Data.Word                  (Word64)

import           Data.OpEnergy.API.V1.Block (BlockHeight)
import           Data.OpEnergy.Account.API.V1.Account
                 ( AccountToken
                 , everifyAccountToken
                 )
import           Data.OpEnergy.Offer.API.V1.OfferID (OfferID)
import           Data.OpEnergy.Offer.API.V1.ContractInfo (ContractID)
import           Data.OpEnergy.Offer.API.V1.OfferSide (OfferSide)
import           Data.OpEnergy.Offer.API.V1.OfferStatus (OfferStatus)

-- | request from the frontend
data LiveRequest
  = LiveRequestInit
    -- ^ @{"action": "init"}@: accepted and ignored. The chain tip, if it is
    -- known, is sent right after 'LiveMessageHello', when the connection
    -- opens
  | LiveRequestAuth AccountToken
    -- ^ @{"action": "auth", "token": "..."}@: additionally receive
    -- notifications about the given account's own offers, contracts and
    -- balance
  | LiveRequestPing
    -- ^ @{"action": "ping"}@: keepalive, answered with 'LiveMessagePong'
  deriving (Eq, Show)

instance FromJSON LiveRequest where
  parseJSON = withObject "LiveRequest" $ \v-> do
    action <- v .: "action"
    case (action :: Text) of
      "init" -> return LiveRequestInit
      "auth" -> do
        rawToken <- v .: "token"
        either (fail . Text.unpack) (return . LiveRequestAuth)
          (everifyAccountToken rawToken)
      "ping" -> return LiveRequestPing
      other -> fail ("LiveRequest: unknown action: " <> show other)

-- | live notification to the frontend
data LiveMessage
  = LiveMessageOfferCreated OfferID
    -- ^ an offer has been posted
  | LiveMessageOfferChanged OfferID OfferStatus Word64
    -- ^ an offer has been accepted, cancelled or has expired. Contains the
    -- offer's new status and matched count
  | LiveMessageContractCreated ContractID OfferID
    -- ^ an offer has been accepted, which created a contract
  | LiveMessageContractSettled ContractID OfferSide Word64
    -- ^ a contract has been settled. Contains the winning side and the
    -- actual mediantime of the contract's target block
  | LiveMessageBlockNew BlockHeight (Maybe Word64)
    -- ^ new chain tip, or a newly known mediantime of the same tip: its
    -- height and the mediantime of the block at that height, @null@ while
    -- the offer service does not know it. On a new tip, confirmations of
    -- live contracts have changed
  | LiveMessageMyChanged
    -- ^ something of the authenticated account has changed: its offers,
    -- contracts or balance. Sent only after 'LiveRequestAuth'
  | LiveMessagePong
    -- ^ answer to 'LiveRequestPing'
  | LiveMessageHello
    -- ^ the first message of every connection, sent with the number of the
    -- last notification published before the connection opened
  deriving (Eq, Show)

-- | a message as it is sent to a connection: with its sequence number
-- (@"seq"@) if it is a notification sent to every connection or
-- 'LiveMessageHello', without one if it is sent to this connection only:
-- the chain tip right after 'LiveMessageHello', 'LiveMessageMyChanged' and
-- 'LiveMessagePong'. Unnumbered messages never cause a gap
data SequencedMessage = SequencedMessage (Maybe Word64) LiveMessage
  deriving (Eq, Show)

instance ToJSON SequencedMessage where
  toJSON (SequencedMessage mseqNo message) = object
    ( maybe id (\seqNo -> (("seq" .= seqNo) :)) mseqNo
      (liveMessagePairs message)
    )

-- | JSON fields of the given notification
liveMessagePairs :: LiveMessage -> [Pair]
liveMessagePairs (LiveMessageOfferCreated offerId) =
  [ "type" .= ("offer.created" :: Text)
  , "offerId" .= offerId
  ]
liveMessagePairs (LiveMessageOfferChanged offerId status matchedCount) =
  [ "type" .= ("offer.changed" :: Text)
  , "offerId" .= offerId
  , "status" .= status
  , "matchedCount" .= matchedCount
  ]
liveMessagePairs (LiveMessageContractCreated contractId offerId) =
  [ "type" .= ("contract.created" :: Text)
  , "contractId" .= contractId
  , "offerId" .= offerId
  ]
liveMessagePairs
    (LiveMessageContractSettled contractId winnerSide actualMtpEpoch) =
  [ "type" .= ("contract.settled" :: Text)
  , "contractId" .= contractId
  , "winnerSide" .= winnerSide
  , "actualMtpEpoch" .= actualMtpEpoch
  ]
liveMessagePairs (LiveMessageBlockNew height mmediantime) =
  [ "type" .= ("block.new" :: Text)
  , "height" .= height
  , "mediantime" .= mmediantime
  ]
liveMessagePairs LiveMessageMyChanged =
  [ "type" .= ("my.changed" :: Text)
  ]
liveMessagePairs LiveMessagePong =
  [ "type" .= ("pong" :: Text)
  ]
liveMessagePairs LiveMessageHello =
  [ "type" .= ("hello" :: Text)
  ]
