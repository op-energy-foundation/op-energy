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
import           Data.OpEnergy.Offer.API.V1.OfferInfo
                 ( OfferID
                 , OfferInfo
                 , PaginatedOffers
                 , PostOfferRequest
                 , PostOfferResult
                 )

-- | all offer endpoints in one API type
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
    :> Description "Posts one or more maker offers, staking \
                   \numberOfOffers*makerStakeSats sats from the caller's \
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
                   \only while it is still open. Refunds the stake."
    :> Post '[JSON] OfferInfo

  :<|> "mine"
    :> Header'
       '[ Required
        , Strict
        , Description "Account token gotten from the account service's \
                      \/login or /register"
        ]
       "Authorization"
       AccountToken
    :> Description "Lists offers posted by the authenticated account, \
                   \newest first."
    :> Get '[JSON] [OfferInfo]

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
    :> Description "Public listing of offers across every account."
    :> Get '[JSON] PaginatedOffers

  :<|> Capture "id" OfferID
    :> Description "Full details for a single offer by id."
    :> Get '[JSON] OfferInfo
