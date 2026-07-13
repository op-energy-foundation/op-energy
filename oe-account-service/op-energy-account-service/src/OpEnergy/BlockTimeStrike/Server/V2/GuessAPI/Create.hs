{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE GADTs                      #-}
module OpEnergy.BlockTimeStrike.Server.V2.GuessAPI.Create
  ( createHandler
  , create
  ) where

import           Control.Monad.Trans.Reader ( asks)
import           Control.Monad.Logger( logError)
import           Control.Monad.Trans
import           Control.Monad.Trans.Except( ExceptT (..))
import           Data.Maybe(fromMaybe)

import           Database.Persist

import           Data.OpEnergy.API.V1.Natural(Natural)
import           Data.OpEnergy.API.V1.Positive
import qualified Data.OpEnergy.API.V1.Block as BlockV1
import qualified Data.OpEnergy.Account.API.V1.Account
                 as AccountV1
import qualified Data.OpEnergy.Account.API.V1.SlowFast as V1
import qualified Data.OpEnergy.BlockTime.API.V2.BlockSpanTimeStrikeGuess
                 as API

import           OpEnergy.Account.Server.V1.Class
                 ( AppM, State(..), runLogging, profile, withDBTransaction
                 , getCurrentHeaderTip
                 )

import           OpEnergy.Error
                   ( eitherThrowJSON, runExceptPrefixT
                   , CallstackError, calculatedGuessesCountNotFound
                   , strikeNotFound, dbQueryError
                   )
import qualified OpEnergy.BlockTimeStrike.Server.V1.BlockTimeStrike
                 as V1
import qualified OpEnergy.BlockTimeStrike.Server.V1.BlockTimeStrikeGuess
                 as V1
import           OpEnergy.ExceptMaybe(exceptTMaybeT)
import qualified OpEnergy.BlockTimeStrike.Server.V1.BlockTimeStrikeGuessService
                 as V1
import qualified OpEnergy.Account.Server.V1.Config as Config
import qualified OpEnergy.BlockTimeStrike.Server.V2.BlockSpanTimeStrikeGuess
                 as BlockSpanTimeStrikeGuess

createHandler
  :: BlockV1.BlockHeight
  -> Natural Int
  -> AccountV1.AccountToken
  -> Maybe (Positive Int)
  -> V1.SlowFast
  -> AppM API.BlockSpanTimeStrikeGuess
createHandler strikeBlock strikeMediantime token mspanSize guess =
    let name = "V2.GuessAPI.Create.createHandler"
    in profile name $ eitherThrowJSON
      ( runLogging . $(logError) )
      $ runExceptPrefixT name $ do
  ExceptT $ create strikeBlock strikeMediantime token mspanSize guess

create
  :: BlockV1.BlockHeight
  -> Natural Int
  -> AccountV1.AccountToken
  -> Maybe (Positive Int)
  -> V1.SlowFast
  -> AppM (Either CallstackError API.BlockSpanTimeStrikeGuess)
create strikeBlock strikeMediantime token mspanSize guess =
    let name = "create"
    in profile name $ runExceptPrefixT name $ do
  (_, latestConfirmedBlock) <- ExceptT getCurrentHeaderTip
  guessV1 <- ExceptT $ V1.createBlockTimeStrikeFutureGuess token strikeBlock
    strikeMediantime guess
  eguessesCount <- exceptTMaybeT
    dbQueryError
    $ withDBTransaction "" $ runExceptPrefixT "DB" $ do
      Entity strikeId _ <- exceptTMaybeT
        strikeNotFound
        $ selectFirst
          [ V1.BlockTimeStrikeBlock ==. strikeBlock
          , V1.BlockTimeStrikeStrikeMediantime ==. fromIntegral strikeMediantime
          ]
          []
      exceptTMaybeT
        calculatedGuessesCountNotFound
        $ selectFirst
          [ V1.CalculatedBlockTimeStrikeGuessesCountStrike ==. strikeId ]
          []
  Entity _ guessesCount <- ExceptT $ return eguessesCount
  spanSize <- lift $ asks
    $ (`fromMaybe` mspanSize)
    . Config.configBlockSpanDefaultSize . config
  ExceptT $ BlockSpanTimeStrikeGuess.apiBlockSpanTimeStrikeGuessModelBlockTimeStrikeGuess
    latestConfirmedBlock
    spanSize
    guessV1
    guessesCount

