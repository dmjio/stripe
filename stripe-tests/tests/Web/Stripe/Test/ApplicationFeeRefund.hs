{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE RankNTypes #-}
module Web.Stripe.Test.ApplicationFeeRefund where

import           Data.Either
import           Test.Hspec
import           Web.Stripe.Test.Prelude
import           Web.Stripe.ApplicationFeeRefund

applicationFeeRefundTests :: StripeSpec
applicationFeeRefundTests stripe = do
  describe "Application Fee Refund tests" $ do
    it "Succesfully fails to refund an unknown application fee" $ do
      result <- stripe $ void $ createApplicationFeeRefund (FeeId "blah")
      result `shouldSatisfy` isLeft
    it "Succesfully fails to retrieve a Application ID with an unknown Refund id" $ do
      result <- stripe $ void $ getApplicationFeeRefund (FeeId "blah") (RefundId "adsf")
      result `shouldSatisfy` isLeft
    it "Succesfully fails to update an ApplicationRefund for an Invalid Fee ID" $ do
      result <- stripe $ void $ updateApplicationFeeRefund (FeeId "blah") (RefundId "blah")
      result `shouldSatisfy` isLeft
    it "Succesfully fails to retrieve all ApplicationRefunds for an Invalid Fee ID" $ do
      result <- stripe $ void $ getApplicationFeeRefunds (FeeId "blah")
      result `shouldSatisfy` isLeft
