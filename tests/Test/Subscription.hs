{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Subscription where

import           Data.Either
import           Control.Monad
import           Data.Maybe
import           Control.Applicative
import qualified Data.Text as T
import           System.Random

import           Test.Config        (getConfig)
import           Test.Hspec
import           Test.Util

import           Web.Stripe
import           Web.Stripe.Subscription
import           Web.Stripe.Customer
import           Web.Stripe.Plan
import           Web.Stripe.Coupon

subscriptionTests :: Spec
subscriptionTests = do
  describe "Subscription tests" $ do
    it "Succesfully creates a Subscription" $ do
      config <- getConfig
      planid <- makePlanId
      result <- stripe config $ do
        Customer { customerId = cid } <- createEmptyCustomer
        void $ createPlan planid
                        0 -- free plan
                        (Currency "usd")
                        Month
                        "sample plan"
                        []
        sub <- createSubscription cid planid []
        void $ deletePlan planid
        void $ deleteCustomer cid
        return sub
      result `shouldSatisfy` isRight
    it "Succesfully retrieves a Subscription" $ do
      config <- getConfig
      planid <- makePlanId
      result <- stripe config $ do
        Customer { customerId = cid } <- createEmptyCustomer
        void $ createPlan planid
                        0 -- free plan
                        (Currency "usd")
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
      config <- getConfig
      planid <- makePlanId
      result <- stripe config $ do
        Customer { customerId = cid } <- createEmptyCustomer
        void $ createPlan planid
                        0 -- free plan
                        (Currency "usd")
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
      config <- getConfig
      planid <- makePlanId
      result <- stripe config $ do
        Customer { customerId = cid } <- createEmptyCustomer
        void $ createPlan planid
                        0 -- free plan
                        (Currency "usd")
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
      config <- getConfig
      planid <- makePlanId
      result <- stripe config $ do
        Customer { customerId = cid } <- createEmptyCustomer
        void $ createPlan planid
                        0 -- free plan
                        (Currency "usd")
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
      config <- getConfig
      planid <- makePlanId
      couponid <- makeCouponId
      result <- stripe config $ do
        Coupon { } <-
          createCoupon
             (Just couponid)
             Once
             (Just $ AmountOff 1)
             (Just $ Currency "usd")
             Nothing
             Nothing
             Nothing
             Nothing
             []
        Customer { customerId = cid } <- createEmptyCustomer
        void $ createPlan planid
                        0 -- free plan
                        (Currency "usd")
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
      config <- getConfig
      planid <- makePlanId
      result <- stripe config $ do
        Customer { customerId = cid } <- createEmptyCustomer
        void $ createPlan planid
                        0 -- free plan
                        (Currency "usd")
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
