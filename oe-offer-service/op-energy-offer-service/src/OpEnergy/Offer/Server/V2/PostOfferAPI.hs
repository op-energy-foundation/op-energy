{-# LANGUAGE GADTs                      #-}
module OpEnergy.Offer.Server.V2.PostOfferAPI
  ( postHandler
  ) where

import           Data.OpEnergy.Offer.API.V1.OfferInfo
                 ( PostOfferRequest, PostOfferResult
                 )
import qualified Data.OpEnergy.Account.API.V1.Account as AccountV1
import           OpEnergy.Offer.Server.V1.Class (AppM)

import qualified OpEnergy.Offer.Server.V2.PostOfferAPI.Post
                 as Post

postHandler
  :: AccountV1.AccountToken
  -> PostOfferRequest
  -> AppM PostOfferResult
postHandler = Post.postHandler
