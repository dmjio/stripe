{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Test.ApplicationFeeRefund where

import           Data.Either
import           Test.Hspec
import           Web.Stripe
import           Web.Stripe.ApplicationFeeRefund

applicationFeeRefundTests :: StripeConfig -> Spec
applicationFeeRefundTests config = do
  describe "Application Fee Refund tests" $ do
    it "Succesfully fails to refund an unknown application fee" $ do
      result <- stripe config $ createApplicationFeeRefund (FeeId "blah") Nothing []
      result `shouldSatisfy` isLeft
    it "Succesfully fails to retrieve a Application ID with an unknown Refund id" $ do
      result <- stripe config $ getApplicationFeeRefund (FeeId "blah") (RefundId "adsf")
      result `shouldSatisfy` isLeft
    it "Succesfully fails to update an ApplicationRefund for an Invalid Fee ID" $ do
      result <- stripe config $ updateApplicationFeeRefund (FeeId "blah") (RefundId "blah") []
      result `shouldSatisfy` isLeft
    it "Succesfully fails to retrieve all ApplicationRefunds for an Invalid Fee ID" $ do
      result <- stripe config $ getApplicationFeeRefunds (FeeId "blah") Nothing Nothing Nothing
      result `shouldSatisfy` isLeft
