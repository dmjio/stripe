{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Test.Customer where

import           Data.Either
import           Test.Config        (getConfig)
import           Test.Hspec
import           Web.Stripe
import           Web.Stripe.Customer

customerTests :: Spec
customerTests =
  describe "Customer tests" $ do
    it "Creates an empty customer" $ do
      config <- getConfig
      result <- stripe config $ do
        Customer{..} <- createEmptyCustomer
        deleteCustomer customerId
      result `shouldSatisfy` isRight
    it "Deletes a customer" $ do
      config <- getConfig
      result <- stripe config $ do
        Customer{..} <- createEmptyCustomer
        deleteCustomer customerId
      result `shouldSatisfy` isRight


