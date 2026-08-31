{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DuplicateRecordFields      #-}
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
      , Description "Account token gotten from the account service's \
                    \/login or /register"
      ]
     "Authorization"
     AccountToken
  :> Description "Cancels the given offer -- only its creator may, and \
                 \only while it's still open -- refunding its stake."
  :> Post '[JSON] OfferInfo
