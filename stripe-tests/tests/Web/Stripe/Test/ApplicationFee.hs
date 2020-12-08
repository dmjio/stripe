{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE RankNTypes #-}
module Web.Stripe.Test.ApplicationFee where

import Test.Hspec
import Web.Stripe.Test.Prelude
import Web.Stripe.ApplicationFee
import Data.Either

applicationFeeTests :: StripeSpec
applicationFeeTests stripe = do
  describe "Application Fee tests" $ do
    it "Succesfully fails to retrieve an unknown application fee" $ do
      result <- stripe $ void $ getApplicationFee (FeeId "fee_unknown")
      result `shouldSatisfy` isLeft
    it "Succesfully retrieves all application fees" $ do
      result <- stripe $ void $ getApplicationFees
      result `shouldSatisfy` isRight
