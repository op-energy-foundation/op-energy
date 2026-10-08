{-- | Messages of the offer service's websocket: requests from the frontend
 - and live notifications about offers, contracts and new blocks.
 -
 - Notifications only say what has changed. The frontend is expected to
 - reload the affected data with the offer API.
 -}
{-# LANGUAGE OverloadedStrings          #-}
module Data.OpEnergy.Offer.API.V1.LiveMessage
  ( LiveRequest(..)
  , LiveMessage(..)
  ) where

import           Data.Aeson
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
    -- ^ @{"action": "init"}@: answered with 'LiveMessageBlockNew' for the
    -- current chain tip, if it is known. Notifications are sent from the
    -- moment the connection is opened, whether or not this is requested
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
  | LiveMessageBlockNew BlockHeight
    -- ^ new chain tip: confirmations of live contracts have changed
  | LiveMessageMyChanged
    -- ^ something of the authenticated account has changed: its offers,
    -- contracts or balance. Sent only after 'LiveRequestAuth'
  | LiveMessagePong
    -- ^ answer to 'LiveRequestPing'
  deriving (Eq, Show)

instance ToJSON LiveMessage where
  toJSON (LiveMessageOfferCreated offerId) = object
    [ "type" .= ("offer.created" :: Text)
    , "offerId" .= offerId
    ]
  toJSON (LiveMessageOfferChanged offerId status matchedCount) = object
    [ "type" .= ("offer.changed" :: Text)
    , "offerId" .= offerId
    , "status" .= status
    , "matchedCount" .= matchedCount
    ]
  toJSON (LiveMessageContractCreated contractId offerId) = object
    [ "type" .= ("contract.created" :: Text)
    , "contractId" .= contractId
    , "offerId" .= offerId
    ]
  toJSON (LiveMessageContractSettled contractId winnerSide actualMtpEpoch) = object
    [ "type" .= ("contract.settled" :: Text)
    , "contractId" .= contractId
    , "winnerSide" .= winnerSide
    , "actualMtpEpoch" .= actualMtpEpoch
    ]
  toJSON (LiveMessageBlockNew height) = object
    [ "type" .= ("block.new" :: Text)
    , "height" .= height
    ]
  toJSON LiveMessageMyChanged = object
    [ "type" .= ("my.changed" :: Text)
    ]
  toJSON LiveMessagePong = object
    [ "type" .= ("pong" :: Text)
    ]
