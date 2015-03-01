{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main where

import           Test.Hspec                 (hspec,parallel)
import           Test.Config                (getConfig)
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
import           Test.Invoice               (invoiceTests)
import           Test.InvoiceItem           (invoiceItemTests)
import           Test.Plan                  (planTests)
import           Test.Recipient             (recipientTests)
import           Test.Refund                (refundTests)
import           Test.Subscription          (subscriptionTests)
import           Test.Token                 (tokenTests)
import           Test.Transfer              (transferTests)
import           Test.Raw                   (rawTest)
import           Test.Event                 (eventTests)
import           Test.Bitcoin               (bitcoinTests)

------------------------------------------------------------------------------
-- | Main test function entry point
main :: IO ()
main = do
  config <- getConfig
  hspec $ parallel $ do
    rawTest config
    bitcoinTests config
    chargeTests config
    refundTests config
    customerTests config
    cardTests config
    subscriptionTests config
    planTests config
    couponTests config
    discountTests config
    invoiceTests config
    invoiceItemTests config
    disputeTests config
    transferTests config
    recipientTests config
    applicationFeeTests config
    applicationFeeRefundTests config
    accountTests config
    balanceTests config
    tokenTests config
    eventTests config



