{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE GADTs                      #-}
module OpEnergy.BlockTimeStrike.Server.V2.GuessAPI.Get
  ( getHandler
  , get
  ) where

import           Data.Text(Text)
import           Control.Monad.Trans.Reader ( asks)
import           Control.Monad.Logger( logError)
import           Control.Monad.Trans
import           Control.Monad.Trans.Except( ExceptT (..))
import           Data.Maybe(fromMaybe)

import           Servant ( err500)

import           Data.OpEnergy.API.V1.Natural(Natural)
import           Data.OpEnergy.API.V1.Positive
import qualified Data.OpEnergy.API.V1.Block as BlockV1
import qualified Data.OpEnergy.Account.API.V1.Account
                 as AccountV1
import qualified Data.OpEnergy.BlockTime.API.V2.BlockSpanTimeStrikeGuess
                 as API

import           OpEnergy.Account.Server.V1.Class
                 ( AppM, State(..), runLogging, profile )

import           OpEnergy.Error( eitherThrowJSON, runExceptPrefixT)
import qualified OpEnergy.BlockTimeStrike.Server.V1.BlockTimeStrikeGuessService
                 as V1
import qualified OpEnergy.Account.Server.V1.Config as Config
import qualified OpEnergy.BlockTimeStrike.Server.V2.BlockSpanTimeStrikeGuess
                 as BlockSpanTimeStrikeGuess

getHandler
  :: BlockV1.BlockHeight
  -> Natural Int
  -> AccountV1.AccountToken
  -> Maybe (Positive Int)
  -> AppM API.BlockSpanTimeStrikeGuess
getHandler strikeBlock strikeMediantime token mspanSize =
    let name = "V2.GuessAPI.Get.getHandler"
    in profile name $ eitherThrowJSON
      (\reason-> do
        callstack <- asks callStack
        runLogging $ $(logError) $ callstack <> ":" <> reason
        return (err500, reason)
      )
      $ runExceptPrefixT name $ do
  ExceptT $ get strikeBlock strikeMediantime token mspanSize

get
  :: BlockV1.BlockHeight
  -> Natural Int
  -> AccountV1.AccountToken
  -> Maybe (Positive Int)
  -> AppM (Either Text API.BlockSpanTimeStrikeGuess)
get strikeBlock strikeMediantime token mspanSize =
    let name = "get"
    in profile name $ runExceptPrefixT name $ do
  guessV1 <- ExceptT $ V1.getBlockTimeStrikeGuess token strikeBlock
    strikeMediantime
  spanSize <- lift $ asks
    $ (`fromMaybe` mspanSize)
    . Config.configBlockSpanDefaultSize . config
  ExceptT $ BlockSpanTimeStrikeGuess.apiBlockSpanTimeStrikeGuessModelBlockTimeStrikeGuess
    spanSize
    guessV1

