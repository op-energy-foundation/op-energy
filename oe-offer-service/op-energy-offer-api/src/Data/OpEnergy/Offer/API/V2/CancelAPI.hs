{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DuplicateRecordFields      #-}
-- | Cancelling one's own offer. Its own Tag module for the same reason as
-- PostOfferAPI: a mutating, stake-moving endpoint kept apart from
-- OffersAPI's reads. Carries no wire types of its own -- it reuses
-- Data.OpEnergy.Offer.API.V2.OffersAPI.OfferInfo as its response.
module Data.OpEnergy.Offer.API.V2.CancelAPI
  ( CancelAPI
  ) where

import           Data.Text                  (Text)
import           Servant.API

import           Data.OpEnergy.Account.API.V1.Account (AccountToken)
import           Data.OpEnergy.Offer.API.V2.OffersAPI (OfferInfo)

type CancelAPI
  = Capture "id" Text
  :> "cancel"
  :> Header'
     '[ Required
      , Strict
      , Description "Account token gotten from the account service's /login or /register"
      ]
     "Authorization"
     AccountToken -- require authentication
  :> Description "Cancels the given offer -- only its creator (identified by the account token) may, and only while it's still \"open\" -- refunding its stake to the creator's wallet balance atomically with the status change, and returning the updated offer. 403 if not the creator, 404 if the offer doesn't exist, 409 if it's not open (already cancelled/expired/etc)."
  :> Post '[JSON] OfferInfo
