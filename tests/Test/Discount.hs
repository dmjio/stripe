{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Discount where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class (liftIO)
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
    -- it "Succesfully deletes a discount from a Customer" $ do
    --   coupon <- makeCouponName
    --   config <- getConfig
    --   result <- stripe config $ do
    --     Coupon { couponId = couponid } <-
    --       createCoupon
    --          (Just $ CouponId coupon)
    --          Once
    --          (Just $ AmountOff 1)
    --          (Just $ Currency "usd")
    --          Nothing
    --          Nothing
    --          Nothing
    --          Nothing
    --          []
    --     Customer { customerId = customerid } <-
    --       createCustomerBase
    --         Nothing
    --         Nothing
    --         Nothing
    --         Nothing
    --         Nothing
    --         Nothing
    --         (Just $ CouponId coupon)
    --         Nothing
    --         Nothing
    --         Nothing
    --         Nothing
    --         Nothing
    --         []
    --     r <- deleteCustomerDiscount customerid
    --     c <- getCustomer customerid
    --     void $ deleteCustomer customerid
    --     void $ deleteCoupon couponid
    --     return (r,c)
    --   result `shouldSatisfy` isRight
    --   let Right (_, Customer{..}) = result
    --   customerDiscount `shouldBe` Nothing
    it "Succesfully deletes a discount from a Subscription" $ do
      coupon <- makeCouponName
      plan <- makeCouponName
      config <- getConfig
      result <- stripe config $ do
        Plan { planId = planid } <-
          createPlan (PlanId plan)
          0 -- free plan
          (Currency "usd")
          Month
          "sample plan"
          []
        liftIO $ print ("plan-id", planid)
        Coupon { couponId = couponid } <-
          createCoupon
             (Just $ CouponId coupon)
             Once
             (Just $ AmountOff 1)
             (Just $ Currency "usd")
             Nothing
             Nothing
             Nothing
             Nothing
             []
        liftIO $ print ("coupon-id", couponid)
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
        liftIO $ print ("customer-id", customerid)
        s@(Subscription { subscriptionId = sid }) <-
          createSubscription customerid (PlanId plan) []
        liftIO $ print ("s", s)
        sub <- updateSubscription customerid sid (Just $ CouponId coupon) []
        liftIO $ print ("sub", sub)
        -- result <- deleteSubscriptionDiscount customerid sid
        -- void $ deletePlan planid
        -- void $ deleteCustomer customerid
        -- void $ deleteCoupon couponid
        return s
      result `shouldSatisfy` isRight




