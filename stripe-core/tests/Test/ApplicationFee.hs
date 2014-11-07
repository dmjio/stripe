{-# LANGUAGE OverloadedStrings #-}
module Test.ApplicationFee where

import Test.Hspec
import Web.Stripe.ApplicationFee
import Web.Stripe
import Data.Either

applicationFeeTests :: StripeConfig -> Spec 
applicationFeeTests config = do
  describe "Application Fee tests" $ do
    it "Succesfully fails to retrieve an unknown application fee" $ do
      result <- stripe config $ getApplicationFee (FeeId "fee_unknown")
      result `shouldSatisfy` isLeft
    it "Succesfully retrieves all application fees" $ do
      result <- stripe config $ getApplicationFees Nothing Nothing Nothing
      result `shouldSatisfy` isRight
