{-# LANGUAGE OverloadedStrings #-}
module Test.Coupon where

import           Data.Either
import           Test.Config        (getConfig)
import           Test.Hspec
import           Web.Stripe
import           Web.Stripe.Account

couponTests :: Spec
couponTests = do
  describe "Plan tests" $ do
    it "Succesfully retrieves a plan" $ do
      config <- getConfig
      result <- stripe config getAccountDetails
      result `shouldSatisfy` isRight

