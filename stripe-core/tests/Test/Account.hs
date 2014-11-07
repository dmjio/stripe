{-# LANGUAGE OverloadedStrings #-}
module Test.Account where

import           Data.Either
import           Test.Hspec
import           Web.Stripe
import           Web.Stripe.Account

accountTests :: StripeConfig -> Spec
accountTests config = do
  describe "Account tests" $ do
    it "Succesfully retrieves account information" $ do
      result <- stripe config getAccountDetails
      result `shouldSatisfy` isRight

