{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE RankNTypes #-}
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
          createCoupon (Just couponName) Once
            -&- (AmountOff 1)
            -&- USD
        void $ deleteCoupon cid
        return c
      result `shouldSatisfy` isRight

    it "Succesfully retrieve a coupon" $ do
      couponName <- makeCouponId
      result <- stripe $ do
                Coupon { couponId = cid } <-
                  createCoupon (Just couponName)
                               Once
                   -&- (AmountOff 1)
                   -&- USD
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
                                  -&- (AmountOff 1)
                                  -&- USD
                void $ deleteCoupon cid
      result `shouldSatisfy` isRight

    it "Succesfully update a coupon" $ do
      couponName <- makeCouponId
      result <- stripe $ do
                Coupon { couponId = cid } <- createCoupon
                                  (Just couponName)
                                  Once
                                  -&-(AmountOff 1)
                                  -&- USD
                r <- updateCoupon cid -&- MetaData [("hi", "there")]
                void $ deleteCoupon cid
                return r
      result `shouldSatisfy` isRight
      let Right (Coupon { couponMetaData = cmd }) = result
      cmd `shouldBe` (MetaData [("hi", "there")])

    it "Succesfully retrieves all coupons" $ do
      result <- stripe $ do void $ getCoupons
      result `shouldSatisfy` isRight

