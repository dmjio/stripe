{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main where

import           Data.ByteString           (ByteString)
import           Data.Either               (isLeft, isRight)
import           Test.Hspec

import           Test.Account              (accountTests)
import           Test.ApplicationFee       (applicationFeeTests)
import           Test.ApplicationFeeRefund (applicationFeeRefundTests)
import           Test.Customer             (customerTests)

------------------------------------------------------------------------------
-- | Main function entry point
main :: IO ()
main = hspec $ do
  -- chargeTests
  -- accountTests
  -- customerTests
  -- applicationFeeTests
  -- applicationFeeRefundTests


