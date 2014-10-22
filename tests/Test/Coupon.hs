{-# LANGUAGE OverloadedStrings #-}
module Test.Coupon where

import           Control.Applicative
import           Control.Monad
import           Data.Either
import qualified Data.Text           as T
import           Data.Text    (Text)
import           System.Random
import           Test.Config         (getConfig)
import           Test.Hspec

import           Web.Stripe
import           Web.Stripe.Coupon

makeCouponName :: IO Text
makeCouponName = T.pack <$> replicateM 10 (randomRIO ('a', 'z'))

couponTests :: Spec
couponTests = do
  describe "Coupon tests" $ do
    it "Succesfully create a coupon" $ do
      config <- getConfig
      couponName <- makeCouponName
      result <- stripe config $ do
        c@(Coupon { couponId = cid }) <-
          createCoupon
             (Just $ CouponId couponName)
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
      config <- getConfig
      couponName <- makeCouponName
      result <- stripe config $ do
                Coupon { couponId = cid } <- createCoupon
                                  (Just $ CouponId couponName)
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
      config <- getConfig
      couponName <- makeCouponName
      result <- stripe config $ do
                Coupon { couponId = cid } <- createCoupon
                                  (Just $ CouponId couponName)
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
      couponName <- makeCouponName
      config <- getConfig
      result <- stripe config $ do
                Coupon { couponId = cid } <- createCoupon
                                  (Just $ CouponId couponName)
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
      config <- getConfig
      result <- stripe config $ getCoupons Nothing Nothing Nothing
      result `shouldSatisfy` isRight






