{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}
module Data.OpEnergy.Offer.API.V1.OffersAPI
  ( OffersAPI
  ) where

import           Servant.API

import           Data.OpEnergy.API.V1.Positive
import           Data.OpEnergy.Account.API.V1.Account
                 ( AccountToken, DisplayName
                 )
import           Data.OpEnergy.Offer.API.V1.OfferStatus
                 ( OfferStatus
                 )
import           Data.OpEnergy.Offer.API.V1.OfferID
                 ( OfferID
                 )
import           Data.OpEnergy.Offer.API.V1.OfferInfo
                 ( OfferInfo
                 , PaginatedOffers
                 , MyOffersResult
                 , PostOfferRequest
                 , PostOfferResult
                 )
import           Data.OpEnergy.Offer.API.V1.ContractInfo
                 ( ContractInfo
                 )
import           Data.OpEnergy.Offer.API.V1.OfferDetails
                 ( OfferDetails
                 )

-- | all offer endpoints in one API type
--
-- Route order: post, cancel, accept, mine, list, getById, getDetails
type OffersAPI
  = "post"
    :> Header'
       '[ Required
        , Strict
        , Description "Account token gotten from the account service's \
                      \/login or /register"
        ]
       "Authorization"
       AccountToken
    :> ReqBody '[JSON] PostOfferRequest
    :> Description "Posts one offer group, staking \
                   \totalContracts*makerStakeSats sats from the caller's \
                   \sandbox wallet balance."
    :> Post '[JSON] PostOfferResult

  :<|> Capture "id" OfferID
    :> "cancel"
    :> Header'
       '[ Required
        , Strict
        , Description "Account token gotten from the account service's \
                      \/login or /register"
        ]
       "Authorization"
       AccountToken
    :> Description "Cancels the given offer. Only its creator may, and \
                   \only while it is still open. Refunds the unfilled \
                   \portion of the stake."
    :> Post '[JSON] OfferInfo

  :<|> Capture "id" OfferID
    :> "accept"
    :> Header'
       '[ Required
        , Strict
        , Description "Account token gotten from the account service's \
                      \/login or /register"
        ]
       "Authorization"
       AccountToken
    :> Description "Accepts one contract from the given offer. Deducts \
                   \takerStakeSats from the caller's balance. The caller \
                   \must not be the offer creator."
    :> Post '[JSON] ContractInfo

  :<|> "mine"
    :> Header'
       '[ Required
        , Strict
        , Description "Account token gotten from the account service's \
                      \/login or /register"
        ]
       "Authorization"
       AccountToken
    :> Description "Returns the authenticated user's offers and \
                   \contracts (as maker or taker)."
    :> Get '[JSON] MyOffersResult

  :<|> "list"
    :> QueryParam'
       '[ Optional
        , Strict
        , Description "restrict to offers currently in this status"
        ]
       "status"
       OfferStatus
    :> QueryParam'
       '[ Optional
        , Strict
        , Description "restrict to offers posted by this display name"
        ]
       "creatorDisplayName"
       DisplayName
    :> QueryParam'
       '[ Optional
        , Strict
        , Description "page number, starting at 1 (default 1)"
        ]
       "page"
       (Positive Int)
    :> QueryParam'
       '[ Optional
        , Strict
        , Description "results per page, default 20, capped at 100"
        ]
       "limit"
       (Positive Int)
    :> Description "Public listing of offers and contracts across \
                   \every account."
    :> Get '[JSON] PaginatedOffers

  :<|> Capture "id" OfferID
    :> Description "Full details for a single offer by id."
    :> Get '[JSON] OfferInfo

  :<|> Capture "id" OfferID
    :> "details"
    :> Description "One offer by id with all its contracts, oldest first, \
                   \whatever the offer's status. 400 for an invalid id, 404 \
                   \for an unknown offer."
    :> Get '[JSON] OfferDetails
