{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE RankNTypes #-}
module Web.Stripe.Test.AllTests where

import           Test.Hspec                            (hspec)
import           Web.Stripe.Test.Config                (getConfig)
import           Web.Stripe.Test.Account               (accountTests)
import           Web.Stripe.Test.ApplicationFee        (applicationFeeTests)
import           Web.Stripe.Test.ApplicationFeeRefund  (applicationFeeRefundTests)
import           Web.Stripe.Test.Balance               (balanceTests)
import           Web.Stripe.Test.Charge                (chargeTests)
import           Web.Stripe.Test.Card                  (cardTests)
import           Web.Stripe.Test.Coupon                (couponTests)
import           Web.Stripe.Test.Customer              (customerTests)
import           Web.Stripe.Test.Discount              (discountTests)
import           Web.Stripe.Test.Dispute               (disputeTests)
import           Web.Stripe.Test.Invoice               (invoiceTests)
import           Web.Stripe.Test.InvoiceItem           (invoiceItemTests)
import           Web.Stripe.Test.Plan                  (planTests)
import           Web.Stripe.Test.Recipient             (recipientTests)
import           Web.Stripe.Test.Refund                (refundTests)
import           Web.Stripe.Test.Subscription          (subscriptionTests)
import           Web.Stripe.Test.Token                 (tokenTests)
import           Web.Stripe.Test.Transfer              (transferTests)
import           Web.Stripe.Test.Event                 (eventTests)
import           Web.Stripe.Test.Prelude               (Stripe)
import           Web.Stripe.Client                     (StripeConfig, StripeError)
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
--    recipientTests stripe
    applicationFeeTests stripe
    applicationFeeRefundTests stripe
    accountTests stripe
    balanceTests stripe
    tokenTests stripe
    eventTests stripe


