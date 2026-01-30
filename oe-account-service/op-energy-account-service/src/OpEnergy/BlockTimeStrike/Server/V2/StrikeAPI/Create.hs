{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE GADTs                      #-}
module OpEnergy.BlockTimeStrike.Server.V2.StrikeAPI.Create
  ( create
  , createHandler
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
import qualified Data.OpEnergy.BlockTime.API.V2.BlockSpanTimeStrike
                 as API
import qualified Data.OpEnergy.Account.API.V1.Account
                 as AccountV1

import           OpEnergy.Account.Server.V1.Class
                 ( AppM, State(..), runLogging, profile )

import           OpEnergy.Error( eitherThrowJSON, runExceptPrefixT)
import qualified OpEnergy.BlockTimeStrike.Server.V1.BlockTimeStrikeService
                 as V1
import qualified OpEnergy.BlockTimeStrike.Server.V1.BlockTimeStrike
                 as V1
import qualified OpEnergy.BlockTimeStrike.Server.V2.BlockSpanTimeStrike
                 as BlockSpanTimeStrike
import qualified OpEnergy.Account.Server.V1.Config as Config

createHandler
  :: AccountV1.AccountToken -- require authentication
  -> BlockV1.BlockHeight
  -> Natural Int
  -> Maybe (Positive Int)
  -> AppM  API.BlockSpanTimeStrike
createHandler token strikeBlock strikeMediantime mspanSize =
    let name = "V2.strikeAPI.Create.createHandler"
    in profile name $ eitherThrowJSON
      (\reason-> do
        callstack <- asks callStack
        runLogging $ $(logError) $ callstack <> ":" <> reason
        return (err500, reason)
      )
      $ runExceptPrefixT name $ do
  ExceptT $ create token strikeBlock strikeMediantime mspanSize

create
  :: AccountV1.AccountToken -- require authentication
  -> BlockV1.BlockHeight
  -> Natural Int
  -> Maybe (Positive Int)
  -> AppM (Either Text API.BlockSpanTimeStrike)
create token strikeBlock strikeMediantime mspanSize =
    let name = "create"
    in profile name $ runExceptPrefixT name $ do
  strikeV1 <- ExceptT $ V1.createBlockTimeStrikeFuture token strikeBlock
    strikeMediantime
  spanSize <- lift $ asks
    $ (`fromMaybe` mspanSize)
    . Config.configBlockSpanDefaultSize . config
  ExceptT $ BlockSpanTimeStrike.apiBlockSpanTimeStrikeModelBlockTimeStrike
    spanSize
    $! V1.apiModelBlockTimeStrike strikeV1 Nothing

