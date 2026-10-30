{-- | What the wallet's arithmetic and its rate limit are supposed to do.
 -
 - These are the pure parts of the money path: what is reserved for a fee,
 - how an amount the wallet answers in millisatoshi becomes satoshi, and how
 - many payments an account may attempt. Each of them has a boundary, which
 - is easy to get wrong in a way no type catches -- a reserve, which rounds
 - to nothing, an amount, which rounds down, a window, which lets one
 - attempt too many through -- so each boundary is written down here.
 -}
{-# LANGUAGE OverloadedStrings          #-}
module Main (main) where

import           Data.Int (Int64)
import           Data.Word (Word64)
import           Database.Persist.Sql (toSqlKey)
import           Test.Hspec

import           Data.OpEnergy.Account.API.V1.Sats (Sats(..))
import           OpEnergy.Account.Server.V1.Person (PersonId)
import           OpEnergy.Account.Server.V1.Wallet.LNBits (msatsToSats)
import           OpEnergy.Account.Server.V1.WalletRateLimit
                 ( newWalletAttempts, takeWalletAttempt
                 )
import           OpEnergy.Account.Server.V2.WalletAPI.PayInvoice (feeReserve)

main :: IO ()
main = hspec $ do
  describe "feeReserve" feeReserveSpec
  describe "msatsToSats" msatsToSatsSpec
  describe "takeWalletAttempt" takeWalletAttemptSpec

-- | the reserve is a share of the amount, rounded up, so a payment, which
-- has an amount at all, always reserves something for a fee
feeReserveSpec :: Spec
feeReserveSpec = do
  let percent = 5 :: Word64
      reserve = feeReserve percent . Sats

  it "reserves nothing for an amount of nothing" $
    reserve 0 `shouldBe` Sats 0

  it "reserves a whole satoshi for an amount, whose share is a fraction" $
    -- 5% of 1 is 0.05, which must not round to nothing: a payment would
    -- then travel with no fee covered at all
    reserve 1 `shouldBe` Sats 1

  it "rounds a share, which is exact, to itself" $
    reserve 20 `shouldBe` Sats 1

  it "rounds a share, which is just above a satoshi, up" $
    reserve 21 `shouldBe` Sats 2

  it "reserves the share of a round amount" $
    reserve 1000 `shouldBe` Sats 50

  it "reserves nothing when the share is nothing" $
    feeReserve 0 (Sats 1000) `shouldBe` Sats 0

  it "reserves the amount itself at a hundred percent" $
    feeReserve 100 (Sats 1000) `shouldBe` Sats 1000

  it "does not wrap for an amount, whose product overflows Word64" $ do
    -- the product of the amount and the percent passes 2^64 well below
    -- the largest amount the DB stores, so it is worked out as Integer.
    -- Worked out in Word64 this answers something small instead
    let huge = 4_000_000_000_000_000_000
    feeReserve 5 (Sats huge) `shouldBe` Sats (huge `div` 20)

-- | LNBits answers in millisatoshi and reports what left the wallet as a
-- negative amount. A fraction of a satoshi rounds up, so nothing is ever
-- reported as less than it is
msatsToSatsSpec :: Spec
msatsToSatsSpec = do
  it "answers nothing for nothing" $
    msatsToSats 0 `shouldBe` Sats 0

  it "rounds a single millisatoshi up to a satoshi" $
    msatsToSats 1 `shouldBe` Sats 1

  it "rounds just under a satoshi up" $
    msatsToSats 999 `shouldBe` Sats 1

  it "answers a whole satoshi as itself" $
    msatsToSats 1000 `shouldBe` Sats 1

  it "rounds just over a satoshi up" $
    msatsToSats 1001 `shouldBe` Sats 2

  it "reads an amount, which left the wallet, as what it was" $
    -- negative is the wallet's way of saying the sats went out, not an
    -- amount below nothing
    msatsToSats (-2500) `shouldBe` Sats 3

  it "does not wrap at the largest amount it can be given" $
    -- adding the 999, which rounds up, overflows Int64 here, so this is
    -- only right if the arithmetic is done in something wider
    msatsToSats (maxBound :: Int64)
      `shouldBe` Sats (fromIntegral ((toInteger (maxBound :: Int64) + 999)
                                     `div` 1000))

  it "does not wrap at the smallest amount it can be given" $
    -- abs of the smallest Int64 is itself, which is still negative
    msatsToSats (minBound :: Int64)
      `shouldBe` Sats (fromIntegral ((abs (toInteger (minBound :: Int64)) + 999)
                                     `div` 1000))

-- | an account may make so many attempts in a window, and the window is
-- dropped rather than slid, so the allowance comes back whole
takeWalletAttemptSpec :: Spec
takeWalletAttemptSpec = do
  let alice = toSqlKey 1 :: PersonId
      bob = toSqlKey 2 :: PersonId
      limit = 3
      accounts = 2
      window = 60

  it "allows the attempts of the window and refuses the one after them" $ do
    attempts <- newWalletAttempts
    let take' = takeWalletAttempt attempts limit accounts window 100
    answers <- mapM (const (take' alice)) [1 :: Int .. 4]
    answers `shouldBe` [True, True, True, False]

  it "counts each account on its own" $ do
    attempts <- newWalletAttempts
    let take' = takeWalletAttempt attempts limit accounts window 100
    _ <- mapM (const (take' alice)) [1 :: Int .. 3]
    take' bob `shouldReturn` True

  it "gives the allowance back once the window is over" $ do
    attempts <- newWalletAttempts
    _ <- mapM (const (takeWalletAttempt attempts limit accounts window 100
                        alice))
              [1 :: Int .. 3]
    -- still inside it
    takeWalletAttempt attempts limit accounts window 159 alice
      `shouldReturn` False
    -- and past it
    takeWalletAttempt attempts limit accounts window 161 alice
      `shouldReturn` True

  it "refuses an account, which does not fit, rather than growing without end" $ do
    attempts <- newWalletAttempts
    let take' who = takeWalletAttempt attempts limit 1 window 100 who
    take' alice `shouldReturn` True
    -- the one account, which fits, is counted; another is refused rather
    -- than making room for itself
    take' bob `shouldReturn` False
    -- and the one, which is counted, still has its allowance
    take' alice `shouldReturn` True
