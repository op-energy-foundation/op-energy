{-- | This module defines events, which are sent to websocket connections
 - through the broadcast channel in 'OpEnergy.Offer.Server.V1.Class.State'.
 -}
module OpEnergy.Offer.Server.V1.LiveEvent
  ( LiveEvent(..)
  ) where

import qualified Data.OpEnergy.Account.API.V1.Account as AccountAPI
import qualified Data.OpEnergy.Account.API.V1.UUID as AccountAPI
import           Data.OpEnergy.Offer.API.V1.LiveMessage (LiveMessage)

-- | one change, which should be announced to websocket connections
data LiveEvent = LiveEvent
  { liveEventMessage :: LiveMessage
    -- ^ sent to every connection
  , liveEventPersons :: [AccountAPI.UUID AccountAPI.Person]
    -- ^ accounts affected by the change. Connections authenticated as one
    -- of them additionally receive 'LiveMessageMyChanged'
  }
