{-# LANGUAGE OverloadedStrings #-}
module Test.ApplicationFee where

import Test.Hspec
import Test.Config
import Web.Stripe.ApplicationFee
import Web.Stripe
import Data.Either

applicationFeeTests :: Spec 
applicationFeeTests = do
  describe "Application Fee tests" $ do
    it "Succesfully fails to retrieve an unknown application fee" $ do
      config <- getConfig
      result <- stripe config $ getApplicationFee (FeeId "fee_4xtEGZhPNDEt3w")
      result `shouldSatisfy` isLeft
    it "Succesfully retrieves all application fees" $ do
      config <- getConfig
      result <- stripe config $ getApplicationFees Nothing Nothing Nothing
      result `shouldSatisfy` isRight
