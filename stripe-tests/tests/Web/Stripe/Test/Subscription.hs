{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
module Web.Stripe.Test.Subscription where

import           Data.Either
import           Data.Maybe

import           Test.Hspec
import           Web.Stripe.Test.Prelude
import           Web.Stripe.Test.Util

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
        Customer { customerId = cid } <- createCustomer
        void $ createPlan planid
                        (Amount 0) -- free plan
                        USD
                        Month
                        (PlanName "sample plan")
        sub <- createSubscription cid planid
        void $ deletePlan planid
        void $ deleteCustomer cid
        return sub
      result `shouldSatisfy` isRight
    it "Succesfully retrieves a Subscription" $ do
      planid <- makePlanId
      result <- stripe $ do
        Customer { customerId = cid } <- createCustomer
        void $ createPlan planid
                        (Amount 0) -- free plan
                        USD
                        Month
                        (PlanName "sample plan")
        Subscription { subscriptionId = sid } <- createSubscription cid planid
        sub <- getSubscription cid sid
        void $ deletePlan planid
        void $ deleteCustomer cid
        return sub
      result `shouldSatisfy` isRight
    it "Succesfully retrieves a Subscription expanded" $ do
      planid <- makePlanId
      result <- stripe $ do
        Customer { customerId = cid } <- createCustomer
        void $ createPlan planid
                        (Amount 0) -- free plan
                        USD
                        Month
                        (PlanName "sample plan")
        Subscription { subscriptionId = sid } <- createSubscription cid planid
        sub <- getSubscription cid sid -&- ExpandParams ["customer"]
        void $ deletePlan planid
        void $ deleteCustomer cid
        return sub
      result `shouldSatisfy` isRight
    it "Succesfully retrieves a Customer's Subscriptions expanded" $ do
      planid <- makePlanId
      result <- stripe $ do
        Customer { customerId = cid } <- createCustomer
        void $ createPlan planid
                        (Amount 0) -- free plan
                        USD
                        Month
                        (PlanName "sample plan")
        void $ createSubscription cid planid
        sub <- getSubscriptions cid -&- ExpandParams ["data.customer"]
        void $ deletePlan planid
        void $ deleteCustomer cid
        return sub
      result `shouldSatisfy` isRight
    it "Succesfully retrieves a Customer's Subscriptions" $ do
      planid <- makePlanId
      result <- stripe $ do
        Customer { customerId = cid } <- createCustomer
        void $ createPlan planid
                        (Amount 0) -- free plan
                        USD
                        Month
                        (PlanName "sample plan")
        void $ createSubscription cid planid
        sub <- getSubscriptions cid
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
             -&- (AmountOff 1)
             -&- USD
        Customer { customerId = cid } <- createCustomer
        void $ createPlan planid
                        (Amount 0) -- free plan
                        USD
                        Month
                        (PlanName "sample plan")
        Subscription { subscriptionId = sid } <- createSubscription cid planid
        sub <- updateSubscription cid sid
                -&- couponid
                -&- MetaData [("hi","there")]
        void $ deletePlan planid
        void $ deleteCustomer cid
        return sub
      result `shouldSatisfy` isRight
      let Right Subscription {..} = result
      subscriptionMetaData `shouldBe` (MetaData [("hi", "there")])
      subscriptionDiscount `shouldSatisfy` isJust
    it "Succesfully cancels a Customer's Subscription" $ do
      planid <- makePlanId
      result <- stripe $ do
        Customer { customerId = cid } <- createCustomer
        void $ createPlan planid
                        (Amount 0) -- free plan
                        USD
                        Month
                        (PlanName "sample plan")
        Subscription { subscriptionId = sid } <- createSubscription cid planid
        sub <- cancelSubscription cid sid -&- AtPeriodEnd False
        void $ deletePlan planid
        void $ deleteCustomer cid
        return sub
      result `shouldSatisfy` isRight
      let Right Subscription {..} = result
      subscriptionStatus `shouldBe` Canceled
