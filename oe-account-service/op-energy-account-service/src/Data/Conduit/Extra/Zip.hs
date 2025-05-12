module Data.Conduit.Extra.Zip
  ( zipConcatMapMC
  ) where

import           Data.Conduit (ConduitT)
import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit.Internal as C(zipSources)

-- | this combinator repeatedly zips the @value0@ gotten from the stream (by the combinator)
-- with a values gotten from the nested conduit, dependently on the @value0@
-- until the nested conduit won't stop producing values. Then, combinator awaits
-- for the next value for @value0@ and reiterate with it
zipConcatMapMC
  :: Monad m
  => (a -> ConduitT () b m ())
  -> ConduitT a (a, b) m ()
zipConcatMapMC nested = C.awaitForever $ \v-> do
    C.toProducer $ C.zipSources
      (C.repeat v)
      (nested v)
