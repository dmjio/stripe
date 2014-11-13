{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards   #-}
module AllTests where

import           Test.Hspec                 (hspec)
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
import           Test.Event                 (eventTests)
import           Test.Prelude               (Stripe)
import           Web.Stripe                 (StripeConfig, StripeError)
------------------------------------------------------------------------------
-- | Main test function entry point
allTests :: (forall a. StripeConfig -> Stripe a -> IO (Either StripeError a))
         -> IO ()
allTests stripe' = do
  config <- getConfig stripe'
  let stripe = stripe' config
  hspec $ do
    chargeTests stripe
    refundTests stripe
    customerTests stripe
    cardTests stripe
    subscriptionTests stripe
    planTests stripe
    couponTests stripe
    discountTests stripe
    invoiceTests stripe
    invoiceItemTests stripe
    disputeTests stripe
    transferTests stripe
    recipientTests stripe
    applicationFeeTests stripe
    applicationFeeRefundTests stripe
    accountTests stripe
    balanceTests stripe
    tokenTests stripe
    eventTests stripe


