{-- | Messages of the offer service's websocket: requests from the frontend
 - and live notifications about offers, contracts, new blocks and the
 - authenticated account's balance.
 -
 - Offer and contract notifications carry the whole offer or contract, as
 - the offer API's public list returns it; a contract's @yourRole@ is
 - @null@. A new block's notification carries the chain tip's height and
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
{-# LANGUAGE OverloadedLists            #-}
module Data.OpEnergy.Offer.API.V1.LiveMessage
  ( LiveRequest(..)
  , LiveMessage(..)
  , SequencedMessage(..)
  ) where

import           Control.Lens               ((&), (.~), (?~))
import           Data.Aeson
import           Data.Aeson.Types           (Pair)
import           Data.Proxy                 (Proxy(..))
import           Data.Swagger
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import           Data.Word                  (Word64)

import           Data.OpEnergy.API.V1.Block (BlockHeight)
import           Data.OpEnergy.Account.API.V1.Account
                 ( AccountToken
                 , defaultAccountToken
                 , everifyAccountToken
                 )
import           Data.OpEnergy.Offer.API.V1.OfferInfo
                 ( OfferInfo
                 , defaultOfferInfo
                 )
import           Data.OpEnergy.Offer.API.V1.ContractInfo (ContractInfo)

-- | request from the frontend
data LiveRequest
  = LiveRequestInit
    -- ^ @{"action": "init"}@: accepted and ignored. The chain tip, if it is
    -- known, is sent right after 'LiveMessageHello', when the connection
    -- opens
  | LiveRequestAuth AccountToken
    -- ^ @{"action": "auth", "token": "..."}@: also receive the account's
    -- new balance as 'LiveMessageMyBalance' whenever the offer service
    -- changes it
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

-- | Hand-written, as swagger has no sum type: the three requests are one
-- object, whose @action@ says which of them it is
instance ToSchema LiveRequest where
  declareNamedSchema _ = do
    tokenSchema <- declareSchemaRef (Proxy :: Proxy AccountToken)
    return $! NamedSchema (Just "LiveRequest") $ mempty
      & type_ ?~ SwaggerObject
      & description ?~ Text.unwords
        [ "Request of the frontend over the websocket."
        , "init is accepted and ignored,"
        , "ping is answered with a pong message, and"
        , "auth takes a token, after which the connection also receives the"
        , "account's my.balance messages."
        ]
      & properties .~
        [ ( "action"
          , Inline $ mempty
            & type_ ?~ SwaggerString
            & enum_ ?~ map String [ "init", "auth", "ping" ]
          )
        , ( "token", tokenSchema )
        ]
      & required .~
        [ "action"
        ]
      & example ?~ object
        [ "action" .= ("auth" :: Text)
        , "token" .= defaultAccountToken
        ]

-- | live notification to the frontend
data LiveMessage
  = LiveMessageOfferCreated OfferInfo
    -- ^ an offer has been posted. Contains the offer as the offer API
    -- returns it
  | LiveMessageOfferChanged OfferInfo
    -- ^ an offer has been accepted, cancelled or has expired. Contains the
    -- offer as it is after the change
  | LiveMessageContractCreated ContractInfo
    -- ^ an offer has been accepted, which created a contract. Contains the
    -- contract; its @yourRole@ is @null@
  | LiveMessageContractSettled ContractInfo
    -- ^ a contract has been settled. Contains the contract after
    -- settlement, with its winning side and the actual mediantime of its
    -- target block; its @yourRole@ is @null@
  | LiveMessageBlockNew BlockHeight (Maybe Word64)
    -- ^ new chain tip, or a newly known mediantime of the same tip: its
    -- height and the mediantime of the block at that height, @null@ while
    -- the offer service does not know it. On a new tip, confirmations of
    -- live contracts have changed
  | LiveMessageMyBalance Word64
    -- ^ new balance, in sats, of the account given in a successful
    -- 'LiveRequestAuth'. Sent to that connection only, right after the
    -- notification of the change that caused it
  | LiveMessagePong
    -- ^ answer to 'LiveRequestPing'
  | LiveMessageHello
    -- ^ the first message of every connection, sent with the number of the
    -- last notification published before the connection opened
  deriving (Eq, Show)

-- | a message as it is sent to a connection: with its sequence number
-- (@"seq"@) if it is a notification sent to every connection or
-- 'LiveMessageHello', without one if it is sent to this connection only:
-- the chain tip right after 'LiveMessageHello', 'LiveMessageMyBalance' and
-- 'LiveMessagePong'. Unnumbered messages never cause a gap
data SequencedMessage = SequencedMessage (Maybe Word64) LiveMessage
  deriving (Eq, Show)

instance ToJSON SequencedMessage where
  toJSON (SequencedMessage mseqNo message) = object
    ( maybe id (\seqNo -> (("seq" .= seqNo) :)) mseqNo
      (liveMessagePairs message)
    )

-- | Hand-written, as swagger has no sum type: every message is one object,
-- whose @type@ says which fields it carries, listed in the description
instance ToSchema SequencedMessage where
  declareNamedSchema _ = do
    -- both @seq@ and @mediantime@ are a number, which can be absent
    optionalNumber <- declareSchemaRef (Proxy :: Proxy (Maybe Word64))
    offerSchema    <- declareSchemaRef (Proxy :: Proxy OfferInfo)
    contractSchema <- declareSchemaRef (Proxy :: Proxy ContractInfo)
    heightSchema   <- declareSchemaRef (Proxy :: Proxy BlockHeight)
    balanceSchema  <- declareSchemaRef (Proxy :: Proxy Word64)
    return $! NamedSchema (Just "SequencedMessage") $ mempty
      & type_ ?~ SwaggerObject
      & description ?~ Text.unwords
        [ "Message of the offer service over the websocket."
        , "Every message has a type, which says which of the fields below it"
        , "carries: offer.created and offer.changed carry offer,"
        , "contract.created and contract.settled carry contract,"
        , "block.new carries height and mediantime,"
        , "my.balance carries balance, and"
        , "hello and pong carry nothing else."
        , "seq numbers the notifications sent to every connection, so a gap"
        , "in the numbers means that one was missed. The messages sent to"
        , "one connection only -- pong, my.balance and the chain tip right"
        , "after hello -- have no seq."
        , "The seq of hello is the number of the last notification published"
        , "before the connection opened, and 0 when none was published yet."
        , "The numbers start again when the service restarts, so they can"
        , "only be compared within one connection: after a reconnect, take"
        , "the seq of the new hello as the baseline and reload the data,"
        , "instead of keeping the one from the connection before."
        ]
      & properties .~
        [ ( "seq", optionalNumber )
        , ( "type"
          , Inline $ mempty
            & type_ ?~ SwaggerString
            & enum_ ?~ map String
              [ "hello"
              , "offer.created"
              , "offer.changed"
              , "contract.created"
              , "contract.settled"
              , "block.new"
              , "my.balance"
              , "pong"
              ]
          )
        , ( "offer", offerSchema )
        , ( "contract", contractSchema )
        , ( "height", heightSchema )
        , ( "mediantime", optionalNumber )
        , ( "balance", balanceSchema )
        ]
      & required .~
        [ "type"
        ]
      & example ?~ toJSON
        ( SequencedMessage (Just 42) (LiveMessageOfferCreated defaultOfferInfo)
        )

-- | JSON fields of the given notification
liveMessagePairs :: LiveMessage -> [Pair]
liveMessagePairs (LiveMessageOfferCreated offer) =
  [ "type" .= ("offer.created" :: Text)
  , "offer" .= offer
  ]
liveMessagePairs (LiveMessageOfferChanged offer) =
  [ "type" .= ("offer.changed" :: Text)
  , "offer" .= offer
  ]
liveMessagePairs (LiveMessageContractCreated contract) =
  [ "type" .= ("contract.created" :: Text)
  , "contract" .= contract
  ]
liveMessagePairs (LiveMessageContractSettled contract) =
  [ "type" .= ("contract.settled" :: Text)
  , "contract" .= contract
  ]
liveMessagePairs (LiveMessageBlockNew height mmediantime) =
  [ "type" .= ("block.new" :: Text)
  , "height" .= height
  , "mediantime" .= mmediantime
  ]
liveMessagePairs (LiveMessageMyBalance balance) =
  [ "type" .= ("my.balance" :: Text)
  , "balance" .= balance
  ]
liveMessagePairs LiveMessagePong =
  [ "type" .= ("pong" :: Text)
  ]
liveMessagePairs LiveMessageHello =
  [ "type" .= ("hello" :: Text)
  ]
