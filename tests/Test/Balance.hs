{-# LANGUAGE OverloadedStrings #-}
module Test.Balance where

import           Data.Either
import           Test.Config        (getConfig)
import           Test.Hspec
import           Web.Stripe
import           Web.Stripe.Account

balanceTests :: Spec
balanceTests = do
  describe "Balance tests" $ do
    it "Succesfully retrieves account information" $ do
      config <- getConfig
      result <- stripe config getAccountDetails
      result `shouldSatisfy` isRight

