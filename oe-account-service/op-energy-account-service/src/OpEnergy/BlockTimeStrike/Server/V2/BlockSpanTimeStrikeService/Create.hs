{-- | This module implements BlockTimeStrike BlockSpanTimeStrikeService.create
 - handler
 -}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GADTs                      #-}
module OpEnergy.BlockTimeStrike.Server.V2.BlockSpanTimeStrikeService.Create
  ( create
  ) where

import           Servant ( err500)
import           Control.Monad.Trans.Reader ( asks)
import           Control.Monad.Logger( logError)
import           Control.Monad.Trans.Except( ExceptT (..))

import qualified Data.OpEnergy.API.V1.Natural as APIV1
import qualified Data.OpEnergy.API.V1.Block
                 as APIV1
import qualified Data.OpEnergy.Account.API.V1.Account
                 as APIV1


import           OpEnergy.Error( eitherThrowJSON, runExceptPrefixT)
import qualified OpEnergy.BlockTimeStrike.Server.V1.BlockTimeStrikeService
                 as V1
import           OpEnergy.Account.Server.V1.Class
                 ( AppM, State(..), runLogging, profile )

-- | this handler returns pageable result of the BlockSpanTimeStrikes for a
-- given spanSize, page and filter
create
  :: APIV1.AccountToken
  -> APIV1.BlockHeight
  -> APIV1.Natural Int
  -> AppM ()
create token height mediantime =
    let name = "V2.create"
    in profile name $ eitherThrowJSON
      (\reason-> do
        callstack <- asks callStack
        runLogging $ $(logError) $ callstack <> ":" <> reason
        return (err500, reason)
      )
      $ runExceptPrefixT name $ do
ExceptT $ V1.createBlockTimeStrikeFuture token height mediantime

