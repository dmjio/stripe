{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader   (ask)
import qualified Data.ByteString.Char8  as B8
import           Data.Monoid
import           Data.String
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as T

import           Web.Stripe.Client

import           Test.Stripe
import           Test.Stripe.Account
import           Test.Stripe.Card
import           Test.Stripe.Charge
import           Test.Stripe.Customer

secretKey :: (IsString m, Monoid m) => m
secretKey = "sk_test_zvqdM2SSA6WwySqM6KJQrqpH"

main :: IO ()
main = do
  B8.putStrLn "Please enter your *TEST* secret key"
  secretKey <- B8.getLine
  B8.putStrLn secretKey
  let config = StripeConfig secretKey "2014-09-08"
  runTests config tests

------------------------------------------------------------------------------
-- | Top level testing function
tests :: StripeTest ()
tests = do
  runCardTests
  runAccountTests
  runCustomerTests
  runChargeTests


