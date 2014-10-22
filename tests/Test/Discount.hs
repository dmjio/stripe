{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Discount where

import           Control.Applicative
import           Control.Monad
import           Data.Either
import           Data.Text              (Text)
import qualified Data.Text              as T
import           System.Random
import           Test.Config            (getConfig)
import           Test.Hspec

import           Web.Stripe
import           Web.Stripe.Coupon
import           Web.Stripe.Customer
import           Web.Stripe.Discount
import           Web.Stripe.Plan
import           Web.Stripe.Subscription

makeCouponName :: IO Text
makeCouponName = T.pack <$> replicateM 10 (randomRIO ('a', 'z'))

discountTests :: Spec
discountTests = do
  describe "Discount tests" $ do
    it "Succesfully deletes a discount from a Customer" $ do
      coupon <- makeCouponName
      config <- getConfig
      result <- stripe config $ do
        Coupon { couponId = couponid } <-
          createCoupon
             (Just $ CouponId coupon)
             Once
             (Just $ AmountOff 1)
             (Just USD)
             Nothing
             Nothing
             Nothing
             Nothing
             []
        Customer { customerId = customerid } <-
          createCustomerBase
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
            (Just $ CouponId coupon)
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
            []
        r <- deleteCustomerDiscount customerid
        c <- getCustomer customerid
        void $ deleteCustomer customerid
        void $ deleteCoupon couponid
        return (r,c)
      result `shouldSatisfy` isRight
      let Right (_, Customer{..}) = result
      customerDiscount `shouldBe` Nothing
    it "Succesfully deletes a discount from a Subscription" $ do
      coupon <- makeCouponName
      plan <- makeCouponName
      config <- getConfig
      result <- stripe config $ do
        Plan { planId = planid } <-
          createPlan (PlanId plan)
          0 -- free plan
          USD
          Month
          "sample plan"
          []
        Coupon { couponId = couponid } <-
          createCoupon
             (Just $ CouponId coupon)
             Once
             (Just $ AmountOff 1)
             (Just USD)
             Nothing
             Nothing
             Nothing
             Nothing
             []
        Customer { customerId = customerid } <-
          createCustomerBase
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
            (Just $ CouponId coupon)
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
            []
        Subscription { subscriptionId = sid } <-
          createSubscription customerid (PlanId plan) []
        void $ updateSubscription customerid sid (Just $ CouponId coupon) []
        result <- deleteSubscriptionDiscount customerid sid
        void $ deletePlan planid
        void $ deleteCustomer customerid
        void $ deleteCoupon couponid
        return result
      result `shouldSatisfy` isRight




