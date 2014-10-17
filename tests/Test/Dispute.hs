{-# LANGUAGE OverloadedStrings #-}
module Test.Dispute where

import           Data.Either
import           Test.Config        (getConfig)
import           Test.Hspec
import           Web.Stripe
import           Web.Stripe.Account

disputeTests :: Spec
disputeTests = do
  describe "Dispute tests" $ do
    it "Succesfully retrieves account information" $ do
      config <- getConfig
      result <- stripe config getAccountDetails
      result `shouldSatisfy` isRight

