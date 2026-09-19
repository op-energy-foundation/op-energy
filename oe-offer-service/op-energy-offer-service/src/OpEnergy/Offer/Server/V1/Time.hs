{-- | Time helpers for timestamps, which are stored in the DB.
 -}
module OpEnergy.Offer.Server.V1.Time
  ( getCurrentTimeDB
  , truncateToMicroseconds
  ) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Time.Clock
                 ( UTCTime(..)
                 , getCurrentTime
                 , diffTimeToPicoseconds
                 , picosecondsToDiffTime
                 )

-- | the given time, truncated to microseconds, the precision of Postgres
-- timestamps: Postgres stores such a time exactly. Truncated, not rounded,
-- so the time stays within its day
truncateToMicroseconds :: UTCTime -> UTCTime
truncateToMicroseconds (UTCTime day time) =
  let picosPerMicro = 1000000
      micros = diffTimeToPicoseconds time `div` picosPerMicro
  in UTCTime day (picosecondsToDiffTime (micros * picosPerMicro))

-- | the current time at the DB's precision, so a timestamp, which is stored
-- and also returned or sent to clients right away, is the same as the one a
-- later read returns
getCurrentTimeDB :: MonadIO m => m UTCTime
getCurrentTimeDB = do
  now <- liftIO getCurrentTime
  return $! truncateToMicroseconds now
