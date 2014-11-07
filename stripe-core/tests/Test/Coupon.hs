{-# LANGUAGE OverloadedStrings #-}
module Test.Coupon where

import           Control.Monad
import           Data.Either
import           Test.Hspec
import           Test.Util

import           Web.Stripe
import           Web.Stripe.Coupon

couponTests :: StripeConfig -> Spec
couponTests config = do
  describe "Coupon tests" $ do
    it "Succesfully create a coupon" $ do
      couponName <- makeCouponId
      result <- stripe config $ do
        c@(Coupon { couponId = cid }) <-
          createCoupon
             (Just couponName)
             Once
             (Just $ AmountOff 1)
             (Just USD)
             Nothing
             Nothing
             Nothing
             Nothing
             []
        void $ deleteCoupon cid
        return c
      result `shouldSatisfy` isRight
    it "Succesfully retrieve a coupon" $ do
      couponName <- makeCouponId
      result <- stripe config $ do
                Coupon { couponId = cid } <- createCoupon
                                  (Just couponName)
                                  Once
                                  (Just $ AmountOff 1)
                                  (Just USD)
                                  Nothing
                                  Nothing
                                  Nothing
                                  Nothing
                                  []
                res <- getCoupon cid
                void $ deleteCoupon cid
                return res
      result `shouldSatisfy` isRight
    it "Succesfully delete a coupon" $ do
      couponName <- makeCouponId
      result <- stripe config $ do
                Coupon { couponId = cid } <- createCoupon
                                  (Just couponName)
                                  Once
                                  (Just $ AmountOff 1)
                                  (Just USD)
                                  Nothing
                                  Nothing
                                  Nothing
                                  Nothing
                                  []
                deleteCoupon cid
      result `shouldSatisfy` isRight
    it "Succesfully update a coupon" $ do
      couponName <- makeCouponId
      result <- stripe config $ do
                Coupon { couponId = cid } <- createCoupon
                                  (Just couponName)
                                  Once
                                  (Just $ AmountOff 1)
                                  (Just USD)
                                  Nothing
                                  Nothing
                                  Nothing
                                  Nothing
                                  []
                r <- updateCoupon cid [("hi", "there")]
                void $ deleteCoupon cid
                return r
      result `shouldSatisfy` isRight
      let Right (Coupon { couponMetaData = cmd }) = result
      cmd `shouldBe` [("hi", "there")]
    it "Succesfully retrieves all coupons" $ do
      result <- stripe config $ getCoupons Nothing Nothing Nothing
      result `shouldSatisfy` isRight






