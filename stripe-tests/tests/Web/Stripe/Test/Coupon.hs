{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
module Web.Stripe.Test.Coupon where

import           Data.Either
import           Test.Hspec
import           Web.Stripe.Test.Prelude
import           Web.Stripe.Test.Util
import           Web.Stripe.Coupon

couponTests :: StripeSpec
couponTests stripe = do
  describe "Coupon tests" $ do
    it "Succesfully create a coupon" $ do
      couponName <- makeCouponId
      result <- stripe $ do
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
      result <- stripe $ do
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
      result <- stripe $ do
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
                void $ deleteCoupon cid
      result `shouldSatisfy` isRight
    it "Succesfully update a coupon" $ do
      couponName <- makeCouponId
      result <- stripe $ do
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
      result <- stripe $ do void $ getCoupons Nothing Nothing Nothing
      result `shouldSatisfy` isRight






