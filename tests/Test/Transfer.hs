{-# LANGUAGE OverloadedStrings #-}
module Test.Transfer where

import           Data.Either
import           Test.Config        (getConfig)
import           Test.Hspec
import           Web.Stripe
import           Web.Stripe.Account

transferTests :: Spec
transferTests = do
  describe "Transfer tests" $ do
    it "Succesfully retrieves account information" $ do
      config <- getConfig
      result <- stripe config getAccountDetails
      result `shouldSatisfy` isRight

