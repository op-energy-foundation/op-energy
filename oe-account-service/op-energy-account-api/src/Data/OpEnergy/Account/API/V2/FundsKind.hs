{-- | Which of an account's three pockets a balance change moves.
 -
 - Free sats are sats an account was given: they can be staked, and they can
 - be won and lost, but they can never be withdrawn.
 -
 - Gift won sats are what the gift won: real sats, which came out of staking
 - sats nobody paid in. They can be withdrawn, but only by an account, which
 - has cleared the withdrawal limits, as they are the way the gift turns
 - into money.
 -
 - Real sats are sats somebody paid in, or won on top of a stake of their
 - own money, and those are the account's to send whenever it likes.
 -
 - One movement belongs to one pocket, so a stake is paid for out of one of
 - the three and never with a mix, and a refund or a payout is returned to
 - the pocket it came out of.
 -}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE OverloadedStrings          #-}
module Data.OpEnergy.Account.API.V2.FundsKind
  ( FundsKind(..)
  , defaultFundsKind
  , fundsKindToText
  , fundsKindFromText
  ) where

import           Data.Aeson
import           Data.Text                  (Text)
import           Data.Typeable              (Typeable)
import           GHC.Generics
import           Control.Lens               ((&), (?~))
import           Data.Swagger
import           Servant.API                (FromHttpApiData(..), ToHttpApiData(..))

-- | the three pockets a balance is made of.
--
-- Ordered as a stake spends them: the gift first, then what the gift won,
-- and an account's own money last.
data FundsKind
  = FreeSats -- ^ given to the account: can be staked, never withdrawn
  | GiftWonSats -- ^ won while staking the gift: withdrawn once the limits are cleared
  | RealSats -- ^ paid in, or won while staking own money: withdrawn freely
  deriving (Show, Eq, Ord, Generic, Typeable, Enum, Bounded)

-- | lowercase serialisation for JSON and query params
fundsKindToText :: FundsKind -> Text
fundsKindToText FreeSats = "free"
fundsKindToText GiftWonSats = "gift_won"
fundsKindToText RealSats = "real"

fundsKindFromText :: Text -> Either Text FundsKind
fundsKindFromText "free" = Right FreeSats
fundsKindFromText "gift_won" = Right GiftWonSats
fundsKindFromText "real" = Right RealSats
fundsKindFromText other  =
  Left $ "FundsKind: unknown kind: " <> other

defaultFundsKind :: FundsKind
defaultFundsKind = RealSats

instance ToJSON FundsKind where
  toJSON = toJSON . fundsKindToText
instance FromJSON FundsKind where
  parseJSON = withText "FundsKind"
    $ either (fail . show) pure . fundsKindFromText
instance ToSchema FundsKind where
  declareNamedSchema _ = pure $ NamedSchema (Just "FundsKind") $ mempty
    & type_ ?~ SwaggerString
    & enum_ ?~ map (toJSON . fundsKindToText) [minBound .. maxBound]
    & example ?~ toJSON defaultFundsKind
instance ToParamSchema FundsKind where
  toParamSchema _ = mempty
    & type_ ?~ SwaggerString
    & enum_ ?~ map (toJSON . fundsKindToText) [minBound .. maxBound]
instance FromHttpApiData FundsKind where
  parseQueryParam = fundsKindFromText
instance ToHttpApiData FundsKind where
  toQueryParam = fundsKindToText
