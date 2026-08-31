{-- |
 - this module exports useful text function
 -}
module Data.Text.Show
  ( tshow
  ) where

import           Data.Text (Text)
import qualified Data.Text as T

tshow :: Show a => a -> Text
tshow some = T.pack $! show some
