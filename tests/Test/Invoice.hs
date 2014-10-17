{-# LANGUAGE OverloadedStrings #-}
module Test.Invoice where

import           Data.Either
import           Test.Config        (getConfig)
import           Test.Hspec
import           Web.Stripe
import           Web.Stripe.Account

invoiceTests :: Spec
invoiceTests = do
  describe "Invoice tests" $ do
    it "Succesfully retrieves account information" $ do
      config <- getConfig
      result <- stripe config getAccountDetails
      result `shouldSatisfy` isRight

