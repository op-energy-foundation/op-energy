{-- | Platform constants for the offer/contract financial model.
 -
 - These are hardcoded in both backend and frontend. If they change,
 - update both together.
 -}
module Data.OpEnergy.Offer.API.V1.Constants
  ( totalPotSats
  , platformFeeSats
  , winnerPayoutSats
  , settlementConfirmations
  , maxContracts
  , minStakeSats
  , maxStakeSats
  ) where

import           Data.Word                  (Word64)

-- | Total sats in every contract pot (makerStake + takerStake).
totalPotSats :: Word64
totalPotSats = 100000

-- | Platform fee deducted from the pot on settlement.
platformFeeSats :: Word64
platformFeeSats = 1000

-- | Sats credited to the winner: 'totalPotSats' - 'platformFeeSats'.
winnerPayoutSats :: Word64
winnerPayoutSats = 99000

-- | Confirmations required before a contract can be settled.
settlementConfirmations :: Word64
settlementConfirmations = 6

-- | Maximum number of contracts per offer group.
maxContracts :: Word64
maxContracts = 20

-- | Minimum maker stake per contract (so takerStake >= 1).
minStakeSats :: Word64
minStakeSats = 1

-- | Maximum maker stake per contract (so takerStake >= 1).
maxStakeSats :: Word64
maxStakeSats = 99999
