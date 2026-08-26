{-# LANGUAGE GADTs                      #-}
module OpEnergy.Offer.Server.V2.PostOfferAPI
  ( handlers
  ) where

import           Servant

import           Data.OpEnergy.Offer.API.V2.PostOfferAPI
import qualified Data.OpEnergy.Account.API.V1.Account as AccountV1
import           OpEnergy.Offer.Server.V1.Class (AppM, AppT)

import qualified OpEnergy.Offer.Server.V2.PostOfferAPI.Post
                 as Post

handlers :: ServerT PostOfferAPI (AppT Handler)
handlers
  = ( Post.postHandler
      :: AccountV1.AccountToken
      -> PostOfferRequest
      -> AppM PostOfferResult
    )
