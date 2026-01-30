{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DuplicateRecordFields      #-}
module Data.OpEnergy.BlockTime.API.V2.GuessAPI where

import           Servant.API

import           Data.OpEnergy.API.V1.Positive
import           Data.OpEnergy.Account.API.V1.Account

import           Data.OpEnergy.BlockTime.API.V2.BlockSpanTimeStrikeGuess

type GuessAPI
  =    Header'
       '[ Required
        , Strict
        , Description "Account token gotten from /login or /register"
        ]
       "Authorization"
       AccountToken -- require authentication
    :> QueryParam'
       '[ Optional
        , Strict
        , Description "defines strikes' block span size"
        ]
       "spanSize"
       (Positive Int)
    :> Description "returns current user's guess for the given blockrate time \
                   \strike"
    :> Get '[JSON] BlockSpanTimeStrikeGuess


