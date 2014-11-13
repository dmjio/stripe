{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
module Test.Subscription where

import           Data.Either
import           Data.Maybe

import           Test.Hspec
import           Test.Prelude
import           Test.Util

import           Web.Stripe.Subscription
import           Web.Stripe.Customer
import           Web.Stripe.Plan
import           Web.Stripe.Coupon

subscriptionTests :: StripeSpec
subscriptionTests stripe = do
  describe "Subscription tests" $ do
    it "Succesfully creates a Subscription" $ do
      planid <- makePlanId
      result <- stripe $ do
        Customer { customerId = cid } <- createEmptyCustomer
        void $ createPlan planid
                        0 -- free plan
                        USD
                        Month
                        "sample plan"
                        []
        sub <- createSubscription cid planid []
        void $ deletePlan planid
        void $ deleteCustomer cid
        return sub
      result `shouldSatisfy` isRight
    it "Succesfully retrieves a Subscription" $ do
      planid <- makePlanId
      result <- stripe $ do
        Customer { customerId = cid } <- createEmptyCustomer
        void $ createPlan planid
                        0 -- free plan
                        USD
                        Month
                        "sample plan"
                        []
        Subscription { subscriptionId = sid } <- createSubscription cid planid []
        sub <- getSubscription cid sid
        void $ deletePlan planid
        void $ deleteCustomer cid
        return sub
      result `shouldSatisfy` isRight
    it "Succesfully retrieves a Subscription expanded" $ do
      planid <- makePlanId
      result <- stripe $ do
        Customer { customerId = cid } <- createEmptyCustomer
        void $ createPlan planid
                        0 -- free plan
                        USD
                        Month
                        "sample plan"
                        []
        Subscription { subscriptionId = sid } <- createSubscription cid planid []
        sub <- getSubscriptionExpandable cid sid ["customer"]
        void $ deletePlan planid
        void $ deleteCustomer cid
        return sub
      result `shouldSatisfy` isRight
    it "Succesfully retrieves a Customer's Subscriptions expanded" $ do
      planid <- makePlanId
      result <- stripe $ do
        Customer { customerId = cid } <- createEmptyCustomer
        void $ createPlan planid
                        0 -- free plan
                        USD
                        Month
                        "sample plan"
                        []
        void $ createSubscription cid planid []
        sub <- getSubscriptionsExpandable cid Nothing Nothing Nothing ["data.customer"]
        void $ deletePlan planid
        void $ deleteCustomer cid
        return sub
      result `shouldSatisfy` isRight
    it "Succesfully retrieves a Customer's Subscriptions" $ do
      planid <- makePlanId
      result <- stripe $ do
        Customer { customerId = cid } <- createEmptyCustomer
        void $ createPlan planid
                        0 -- free plan
                        USD
                        Month
                        "sample plan"
                        []
        void $ createSubscription cid planid []
        sub <- getSubscriptions cid Nothing Nothing Nothing 
        void $ deletePlan planid
        void $ deleteCustomer cid
        return sub
      result `shouldSatisfy` isRight
    it "Succesfully updates a Customer's Subscriptions" $ do
      planid <- makePlanId
      couponid <- makeCouponId
      result <- stripe $ do
        Coupon { } <-
          createCoupon
             (Just couponid)
             Once
             (Just $ AmountOff 1)
             (Just USD)
             Nothing
             Nothing
             Nothing
             Nothing
             []
        Customer { customerId = cid } <- createEmptyCustomer
        void $ createPlan planid
                        0 -- free plan
                        USD
                        Month
                        "sample plan"
                        []
        Subscription { subscriptionId = sid } <- createSubscription cid planid []
        sub <- updateSubscription cid sid (Just couponid) [("hi","there")]
        void $ deletePlan planid
        void $ deleteCustomer cid
        return sub
      result `shouldSatisfy` isRight
      let Right Subscription {..} = result
      subscriptionMetaData `shouldBe` [("hi", "there")]
      subscriptionDiscount `shouldSatisfy` isJust
    it "Succesfully cancels a Customer's Subscription" $ do
      planid <- makePlanId
      result <- stripe $ do
        Customer { customerId = cid } <- createEmptyCustomer
        void $ createPlan planid
                        0 -- free plan
                        USD
                        Month
                        "sample plan"
                        []
        Subscription { subscriptionId = sid } <- createSubscription cid planid []
        sub <- cancelSubscription cid sid False
        void $ deletePlan planid
        void $ deleteCustomer cid
        return sub
      result `shouldSatisfy` isRight
      let Right Subscription {..} = result
      subscriptionStatus `shouldBe` Canceled
