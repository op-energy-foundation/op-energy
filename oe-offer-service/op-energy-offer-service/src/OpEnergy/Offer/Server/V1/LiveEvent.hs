{-- | This module defines events, which are sent to websocket connections
 - through the broadcast channel in 'OpEnergy.Offer.Server.V1.Class.State'.
 -}
module OpEnergy.Offer.Server.V1.LiveEvent
  ( LiveEvent(..)
  , changedBalance
  ) where

import           Data.Word (Word64)

import qualified Data.OpEnergy.Account.API.V1.Account as AccountAPI
import qualified Data.OpEnergy.Account.API.V1.UUID as AccountAPI
import           Data.OpEnergy.Account.API.V1.Sats (Sats(..))
import           Data.OpEnergy.Offer.API.V1.LiveMessage (LiveMessage)

-- | one change, which should be announced to websocket connections
data LiveEvent = LiveEvent
  { liveEventMessage :: LiveMessage
    -- ^ sent to every connection
  , liveEventBalances :: [(AccountAPI.UUID AccountAPI.Person, Word64)]
    -- ^ the balances this change has set: each is sent as
    -- 'LiveMessageMyBalance' to the connections authenticated as its account
  }

-- | 'liveEventBalances' entry for the given account after an attempt to
-- change its balance: the new balance, or none if the attempt failed
changedBalance
  :: AccountAPI.UUID AccountAPI.Person
  -> Either e Sats
  -> [(AccountAPI.UUID AccountAPI.Person, Word64)]
changedBalance person =
  either (const []) (\(Sats balance) -> [(person, balance)])
