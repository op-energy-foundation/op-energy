{-- | Which of an account's two pockets a balance change moves.
 -
 - Free sats are sats an account was given: they can be staked, and they can
 - be won and lost, but they can never be withdrawn. Real sats are sats
 - somebody paid in, or won on top of a stake, and those can.
 -
 - One movement belongs to one pocket, so a stake is paid for with free sats
 - or with real sats and never with a mix of both, and a refund or a payout
 - can be returned to the pocket it came out of.
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

-- | the two pockets a balance is made of
data FundsKind
  = FreeSats -- ^ given to the account: can be staked, never withdrawn
  | RealSats -- ^ paid in or won on top of a stake: can be withdrawn
  deriving (Show, Eq, Ord, Generic, Typeable, Enum, Bounded)

-- | lowercase serialisation for JSON and query params
fundsKindToText :: FundsKind -> Text
fundsKindToText FreeSats = "free"
fundsKindToText RealSats = "real"

fundsKindFromText :: Text -> Either Text FundsKind
fundsKindFromText "free" = Right FreeSats
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
