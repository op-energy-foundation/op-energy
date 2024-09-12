-- | this module defines Context data type and related routine. This data type's
-- goal is to provide more context around nested types, which can be used to
-- limit input of functions in order to force caller to provide proper checks/
-- validations
-- Example:
--
--   import Context as Context
--
--   data NotNull
--   verifyNotNull :: Int -> Context NotNull Int
--   verifyNotNull input = if input > 0 then Context.believeme input else undefined
--
--   divide:: Int -> NotNull Int -> Int
--   divide l r = l `div` (unContext r)
--
--   main = do
--     let
--         l = 10
--         l1 = 20
--         r = verifyNotNull 5
--     print $ divide l r
--     print $ divide l1 r
--
-- so not null check can be performed only once here by forcing type
-- Context NotNull
module OpEnergy.BlockTimeStrike.Server.V1.Context
  ( Context
  , unContext
  , believeme
  ) where

newtype Context a b = Context
  { unContext :: b
  }

-- | such name is just to have a notion mark in the code where do we decide that
-- there were enough checks and we can wrap data type in context now.
-- needless to say that this function is the same as unsafeCoerce and etc
-- so you verifyNotNull can look like this:
--   verifyNotNull :: Int -> Context NotNull Int
--   verifyNotNull input = Context.believeme input
-- so in this context, Context data type relies on a correctness of functions,
-- that use believeme function.
believeme :: b -> Context a b
believeme = Context
