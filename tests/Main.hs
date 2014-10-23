{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main where

import           Test.Hspec

import           Test.Account               (accountTests)
import           Test.ApplicationFee        (applicationFeeTests)
import           Test.ApplicationFeeRefund  (applicationFeeRefundTests)
import           Test.Balance               (balanceTests)
import           Test.Charge                (chargeTests)
import           Test.Card                  (cardTests)
import           Test.Coupon                (couponTests)
import           Test.Customer              (customerTests)
import           Test.Discount              (discountTests)
import           Test.Dispute               (disputeTests)
-- import           Test.Invoice               (invoiceTests)
import           Test.InvoiceItem           (invoiceItemTests)
import           Test.Plan                  (planTests)
import           Test.Recipient             (recipientTests)
import           Test.Refund                (refundTests)
import           Test.Subscription          (subscriptionTests)
import           Test.Token                 (tokenTests)
import           Test.Transfer              (transferTests)
-- import           Test.Event                 (eventTests)


------------------------------------------------------------------------------
-- | Main test function entry point
main :: IO ()
main = hspec $ do
  chargeTests
  refundTests
  customerTests
  cardTests
  subscriptionTests
  planTests
  couponTests
  discountTests
  invoiceTests
  invoiceItemTests
  disputeTests
  transferTests
  recipientTests
  applicationFeeTests
  applicationFeeRefundTests
  accountTests
  balanceTests
  tokenTests


