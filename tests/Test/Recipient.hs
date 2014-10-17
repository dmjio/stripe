{-# LANGUAGE OverloadedStrings #-}
module Test.Recipient where

import           Data.Either
import           Test.Config        (getConfig)
import           Test.Hspec
import           Web.Stripe
import           Web.Stripe.Account

recipientTests :: Spec
recipientTests = do
  describe "Recipient tests" $ do
    it "Succesfully retrieves account information" $ do
      config <- getConfig
      result <- stripe config getAccountDetails
      result `shouldSatisfy` isRight

