{-- | This module implements BlockTime strike service.
 -}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GADTs                      #-}
module OpEnergy.BlockTimeStrike.Server.V2.BlockSpanTimeStrikeGuessService.Create
  ( createHandler
  ) where

import           Data.Text(Text)

import           Servant ( err500)
import           Control.Monad.Trans.Reader ( asks)
import           Control.Monad.Logger( logError)
import           Control.Monad.Trans
import           Control.Monad.Trans.Except( ExceptT (..))

import           Data.OpEnergy.API.V1.Natural
import qualified Data.OpEnergy.API.V1.Block
                 as APIV1
import qualified Data.OpEnergy.Account.API.V1.Account
                 as APIV1
import qualified Data.OpEnergy.Account.API.V1.SlowFast
                 as APIV1

import           Data.OpEnergy.BlockTime.API.V2.BlockSpanTimeStrikeGuess

import           OpEnergy.Error( eitherThrowJSON, runExceptPrefixT)
import qualified OpEnergy.BlockTimeStrike.Server.V1.BlockTimeStrikeGuessService
                 as V1
import qualified OpEnergy.Account.Server.V1.Config as Config
import           OpEnergy.Account.Server.V1.Class
                 ( AppM, State(..), runLogging, profile )
import qualified OpEnergy.BlockTimeStrike.Server.V2.BlockSpanTimeStrikeGuess
                 as BlockSpanTimeStrikeGuess
createHandler
  :: APIV1.AccountToken -- require authentication
  -> APIV1.BlockHeight
  -> Natural Int
  -> APIV1.SlowFast
  -> AppM BlockSpanTimeStrikeGuess
createHandler token strikeBlockHeight strikeMediantime guess =
    let name = "BlockSpanTimeStrikeGuessService.createHandler"
    in profile name $ eitherThrowJSON
      ( \reason-> do
        callstack <- asks callStack
        runLogging $ $(logError) $ callstack <> ": " <> reason
        return (err500, reason)
      )
      $ create token strikeBlockHeight strikeMediantime guess

create
  :: APIV1.AccountToken -- require authentication
  -> APIV1.BlockHeight
  -> Natural Int
  -> APIV1.SlowFast
  -> AppM (Either Text BlockSpanTimeStrikeGuess)
create token strikeBlockHeight strikeMediantime guess =
    let name = "BlockSpanTimeStrikeGuessService.create"
    in profile name $ runExceptPrefixT name $ do
  guessV1 <- ExceptT $ V1.createBlockTimeStrikeFutureGuess token
    strikeBlockHeight strikeMediantime guess
  spanSize <- lift $ asks $ Config.configBlockSpanDefaultSize . config
  ExceptT
    $ BlockSpanTimeStrikeGuess.apiBlockSpanTimeStrikeGuessModelBlockTimeStrikeGuess
      spanSize guessV1


