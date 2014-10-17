{-# LANGUAGE OverloadedStrings #-}
module Test.InvoiceItem where

import           Data.Either
import           Test.Config        (getConfig)
import           Test.Hspec
import           Web.Stripe
import           Web.Stripe.Account

invoiceItemTests :: Spec
invoiceItemTests = do
  describe "Invoice item tests" $ do
    it "Succesfully retrieves a plan" $ do
      config <- getConfig
      result <- stripe config getAccountDetails
      result `shouldSatisfy` isRight

