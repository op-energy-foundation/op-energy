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
import qualified Data.List as List
import           Data.Word (Word64)
import           Database.Persist.Sql (toSqlKey)
import           Test.Hspec

import           Data.OpEnergy.Account.API.V1.Sats (Sats(..))
import           Data.OpEnergy.Account.API.V2.Bolt11Invoice
                 ( defaultBolt11Invoice
                 )
import           Data.OpEnergy.Account.API.V2.PaymentHash
                 ( defaultPaymentHash
                 )
import           Data.OpEnergy.Account.API.V2.WalletMode (WalletMode(..))
import           OpEnergy.Account.Server.V1.Config
                 ( Config(..), defaultConfig, everifyWalletConfig
                 )
import           OpEnergy.Account.Server.V1.Person (PersonId)
import           OpEnergy.Account.Server.V1.Wallet.LNBits (msatsToSats)
import           OpEnergy.Account.Server.V1.Wallet.Mock (invoiceIsPayable)
import           OpEnergy.Account.Server.V1.WalletInvoiceReservation
                 ( hasInvoiceSlot
                 )
import           OpEnergy.Account.Server.V1.WalletPayment
import           OpEnergy.Account.Server.V1.WalletRateLimit
                 ( newWalletAttempts, takeWalletAttempt
                 )
import           OpEnergy.Account.Server.V1.WalletReconcile
                 ( UnknownOutgoing(..), decideUnknownOutgoing
                 , invoiceIsWrittenOff
                 )
import           OpEnergy.Account.Server.V2.WalletAPI.GetTransactions
                 ( pageOffset
                 )
import           OpEnergy.Account.Server.V2.WalletAPI.PayInvoice (feeReserve)

main :: IO ()
main = hspec $ do
  describe "feeReserve" feeReserveSpec
  describe "msatsToSats" msatsToSatsSpec
  describe "takeWalletAttempt" takeWalletAttemptSpec
  describe "pageOffset" pageOffsetSpec
  describe "invoiceIsPayable" invoiceIsPayableSpec
  describe "decideUnknownOutgoing" decideUnknownOutgoingSpec
  describe "invoiceIsWrittenOff" invoiceIsWrittenOffSpec
  describe "everifyWalletConfig" everifyWalletConfigSpec
  describe "hasInvoiceSlot" hasInvoiceSlotSpec

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

-- | the offset of a page is the page number times the size of a page, and
-- the page number is the caller's, so the product is checked rather than
-- trusted
pageOffsetSpec :: Spec
pageOffsetSpec = do
  it "starts the first page at nothing" $
    pageOffset 0 10 `shouldBe` Just 0

  it "offsets a later page by the pages before it" $
    pageOffset 2 10 `shouldBe` Just 20

  it "offsets the largest page, which still fits" $
    let page = (maxBound :: Int) `div` 10
    in pageOffset page 10 `shouldBe` Just (page * 10)

  it "refuses a page, whose offset does not fit" $
    -- this page number times the default page size passes 2^64 and wraps
    -- to 4, which answers a page, which should hold nothing, with the four
    -- newest movements of the account
    pageOffset 1844674407370955162 10 `shouldBe` Nothing

  it "refuses the largest page number there is" $
    pageOffset (maxBound :: Int) 10 `shouldBe` Nothing

-- | an invoice is payable while it is waiting and its expiry has not gone
-- by. The expiry was recorded and never read, so an invoice stayed payable
-- for good
invoiceIsPayableSpec :: Spec
invoiceIsPayableSpec = do
  let invoice = WalletPayment
        { walletPaymentPersonId = toSqlKey 1
        , walletPaymentDirection = Incoming
        , walletPaymentStatus = Pending
        , walletPaymentAmountSats = Sats 1000
        , walletPaymentFeeSats = Sats 0
        , walletPaymentPaymentHash = defaultPaymentHash
        , walletPaymentInvoice = defaultBolt11Invoice
        , walletPaymentNote = Nothing
        , walletPaymentExpiresAt = Just 200
        , walletPaymentCreditedAt = Nothing
        , walletPaymentMadeAtWallet = Just "mock"
        , walletPaymentUnknownSince = Nothing
        , walletPaymentCreatedAt = 100
        , walletPaymentUpdatedAt = 100
        }

  it "is payable while it waits and its expiry is ahead" $
    invoiceIsPayable 150 invoice `shouldBe` True

  it "is payable at the moment it expires" $
    invoiceIsPayable 200 invoice `shouldBe` True

  it "is not payable once its expiry has gone by" $
    invoiceIsPayable 201 invoice `shouldBe` False

  it "is payable while it waits and was given no expiry" $
    invoiceIsPayable 100000 invoice { walletPaymentExpiresAt = Nothing }
      `shouldBe` True

  it "is not payable once it has been paid" $
    invoiceIsPayable 150 invoice { walletPaymentStatus = Settled }
      `shouldBe` False

  it "is not payable once it has been written off" $
    invoiceIsPayable 150 invoice { walletPaymentStatus = Expired }
      `shouldBe` False

-- | the sats of an outgoing payment, which the wallet does not have, are
-- given back once it can no longer be in flight -- counted from the first
-- answer, which did not have it, not from when the payment was made
decideUnknownOutgoingSpec :: Spec
decideUnknownOutgoingSpec = do
  let grace = 60
      thisWallet = "lnbits:aaaa"
      -- the payment was made at the wallet, which is being asked
      decide now munknownSince = decideUnknownOutgoing grace now
        (Just thisWallet) thisWallet munknownSince

  it "records when the wallet first did not have it" $
    decide 1000 Nothing `shouldBe` NoteUnknown

  it "waits while it has not been unknown for long" $
    decide 1030 (Just 1000) `shouldBe` WaitLonger

  it "waits at the moment the grace period is up" $
    decide 1060 (Just 1000) `shouldBe` WaitLonger

  it "gives the sats back once it has been unknown for longer" $
    decide 1061 (Just 1000) `shouldBe` GiveBack

  it "records the first unknown answer about an old payment rather than \
     \giving its sats back at once" $
    -- the payment was made long before the grace period and the wallet
    -- reported it as pending all that while. Counted from when it was
    -- made, this first answer, which does not have it, would give its
    -- sats back, while it may still settle
    decide 100000 Nothing `shouldBe` NoteUnknown

  it "leaves a payment of another wallet for a person, however long it \
     \has been unknown" $
    -- the sats left the wallet it was made at. The wallet being asked
    -- never knew the payment and never will, so no wait makes its answer
    -- evidence the payment did not happen
    decideUnknownOutgoing grace 100000 (Just "lnbits:bbbb") thisWallet
      (Just 1000) `shouldBe` LeaveForAPerson

  it "leaves a payment of another wallet for a person on the first \
     \unknown answer too" $
    decideUnknownOutgoing grace 1000 (Just "lnbits:bbbb") thisWallet
      Nothing `shouldBe` LeaveForAPerson

  it "leaves a payment, whose wallet was never recorded, for a person" $
    -- it may be a payment this wallet lost, or one of a wallet used
    -- before it; nothing recorded says which
    decideUnknownOutgoing grace 100000 Nothing thisWallet (Just 1000)
      `shouldBe` LeaveForAPerson

  it "treats the mock wallet as the other wallet it is" $
    -- a deployment, which ran the mock and was then pointed at a node,
    -- asks lnbits about payments, which were only ever in the mock
    decideUnknownOutgoing grace 100000 (Just "mock") thisWallet
      (Just 1000) `shouldBe` LeaveForAPerson

-- | an invoice nobody paid is written off a grace period after its expiry,
-- so a payment, which arrived just before it, is still seen
invoiceIsWrittenOffSpec :: Spec
invoiceIsWrittenOffSpec = do
  let grace = 60
      writtenOff = invoiceIsWrittenOff grace

  it "does not write off an invoice, which was given no expiry" $
    writtenOff 100000 Nothing `shouldBe` False

  it "does not write off an invoice, which has not expired" $
    writtenOff 150 (Just 200) `shouldBe` False

  it "does not write off an invoice, which has only just expired" $
    -- it is asked about for a while longer: a payment, which arrived just
    -- before the expiry, is reported after it, and an invoice, which is
    -- written off, is never asked about again and never credited
    writtenOff 201 (Just 200) `shouldBe` False

  it "does not write off an invoice at the moment its grace is up" $
    writtenOff 260 (Just 200) `shouldBe` False

  it "writes off an invoice, which stayed unpaid past its grace" $
    writtenOff 261 (Just 200) `shouldBe` True

-- | the wallet options are checked against each other at startup, so a
-- config, which can not work -- or which would hand out real sats for
-- nothing -- stops the service rather than the first request it breaks
everifyWalletConfigSpec :: Spec
everifyWalletConfigSpec = do
  let salted = defaultConfig { configSalt = "a secret" }
      lnbits = salted
        { configWalletBackend = WalletModeLNBits
        , configLNBitsInvoiceKey = "invoice key"
        , configLNBitsAdminKey = "admin key"
        , configStartingBalanceSats = 0
        }
      complainsAbout about config = case everifyWalletConfig config of
        Left problems -> about `List.isInfixOf` problems
        Right () -> False

  it "accepts the defaults once a salt is chosen" $
    everifyWalletConfig salted `shouldBe` Right ()

  it "refuses a config without a salt" $
    -- the identifier of a ledger entry is its row id hashed with the salt,
    -- and ids of a shared sequence hashed with nothing can be worked back
    defaultConfig `shouldSatisfy` complainsAbout "SECRET_SALT"

  it "accepts a real wallet, which grants nothing at signup" $
    everifyWalletConfig lnbits `shouldBe` Right ()

  it "refuses a real wallet together with a signup grant" $
    -- every account is credited with the grant without any sats being paid
    -- in for it, so it could be withdrawn from the wallet as real sats
    lnbits { configStartingBalanceSats = 300000 }
      `shouldSatisfy` complainsAbout "STARTING_BALANCE_SATS"

  it "allows a signup grant on the wallet, which moves no real sats" $
    everifyWalletConfig salted { configStartingBalanceSats = 300000 }
      `shouldBe` Right ()

-- | the slots held for the invoices, which are being made right now, count
-- against the limit alongside the ones already waiting, so a burst of
-- requests can not each take the last slot
hasInvoiceSlotSpec :: Spec
hasInvoiceSlotSpec = do
  let limit = 10
      room = hasInvoiceSlot limit

  it "has room while fewer than the limit are waiting" $
    room 9 0 `shouldBe` True

  it "has no room at the limit" $
    room 10 0 `shouldBe` False

  it "has no room past the limit" $
    room 11 0 `shouldBe` False

  it "counts a slot, which is held for a call in flight" $
    -- nine waiting and one being made is already the limit: without
    -- counting the one in flight this answers True and the burst, which
    -- is mid-call, takes an eleventh slot
    room 9 1 `shouldBe` False

  it "counts the slots held by a whole burst" $
    room 0 10 `shouldBe` False

  it "has room while the slots held leave one" $
    room 4 5 `shouldBe` True

  it "has no room at all when the limit is nothing" $
    hasInvoiceSlot 0 0 0 `shouldBe` False
