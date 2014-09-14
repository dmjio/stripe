{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Reader (ask)
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Char8      as B8
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T

import           Web.Stripe.Client

import           Test.Stripe
import           Test.Stripe.Account
import           Test.Stripe.Card
import           Test.Stripe.Charge
import           Test.Stripe.Customer

import           Test.Hspec

secretKey :: ByteString
secretKey = "sk_test_zvqdM2SSA6WwySqM6KJQrqpH"

main :: IO ()
main = do
  B8.putStrLn "Please enter your *TEST* secret key"
  secretKey <- B8.getLine
  B8.putStrLn secretKey
  let config = StripeConfig secretKey "2014-09-08"
  hspec $ do
    describe "Prelude.head" $ do
      it "returns the first element of a list" $ do
        head [23 ..] `shouldBe` (23 :: Int)
  runTests config tests

------------------------------------------------------------------------------
-- | Top level testing function
tests :: StripeTest ()
tests = do
  runCardTests
  runAccountTests
  runCustomerTests
  runChargeTests


