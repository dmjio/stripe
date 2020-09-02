{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE RankNTypes #-}
module Web.Stripe.Test.Discount where

import           Data.Either
import           Test.Hspec
import           Web.Stripe.Test.Util
import           Web.Stripe.Test.Prelude

import           Web.Stripe.Coupon
import           Web.Stripe.Customer
import           Web.Stripe.Discount
import           Web.Stripe.Plan
import           Web.Stripe.Subscription

discountTests :: StripeSpec
discountTests stripe = do
  describe "Discount tests" $ do
    it "Succesfully deletes a discount from a Customer" $ do
      coupon <- makeCouponId
      result <- stripe $ do
        Coupon { couponId = couponid } <-
          createCoupon
             (Just coupon)
             Once
             -&- (AmountOff 1)
             -&- USD
        Customer { customerId = customerid } <-
          createCustomer -&- couponid
        r <- deleteCustomerDiscount customerid
        c <- getCustomer customerid
        void $ deleteCustomer customerid
        void $ deleteCoupon couponid
        return (r,c)
      result `shouldSatisfy` isRight
      let Right (_, Customer{..}) = result
      customerDiscount `shouldBe` Nothing
    it "Succesfully deletes a discount from a Subscription" $ do
      coupon <- makeCouponId
      plan   <- makePlanId
      result <- stripe $ do
        Plan { planId = planid } <-
          createPlan plan
          (Amount 0) -- free plan
          USD
          Month
          (PlanName "sample plan")
        Coupon { couponId = couponid } <-
          createCoupon
             (Just coupon)
             Once
             -&- (AmountOff 1)
             -&- USD
        Customer { customerId = customerid } <-
          createCustomer
            -&- coupon
        Subscription { subscriptionId = sid } <-
          createSubscription customerid plan
        void $ updateSubscription customerid sid -&- coupon
        result <- deleteSubscriptionDiscount customerid sid
        void $ deletePlan planid
        void $ deleteCustomer customerid
        void $ deleteCoupon couponid
        return result
      result `shouldSatisfy` isRight




