{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Test.Customer where

import           Data.Either
import           Data.Maybe
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
        c@Customer{..} <- createEmptyCustomer
        _ <- deleteCustomer customerId
        return c
      result `shouldSatisfy` isRight
    it "Deletes a customer" $ do
      config <- getConfig
      result <- stripe config $ do
        c@Customer{..} <- createEmptyCustomer
        _ <- deleteCustomer customerId
        return c
      result `shouldSatisfy` isRight
    it "Gets a customer" $ do
      config <- getConfig
      result <- stripe config $ do
        Customer { customerId = cid } <- createEmptyCustomer
        customer <- getCustomer cid
        _ <- deleteCustomer cid
        return customer
      result `shouldSatisfy` isRight
    it "Gets a customer expandable" $ do
      config <- getConfig
      result <- stripe config $ do
        Customer { customerId = cid } <- createEmptyCustomer
        customer <- getCustomerExpandable cid ["default_card"]
        _ <- deleteCustomer cid
        return customer
      result `shouldSatisfy` isRight
    it "Gets customers" $ do
      config <- getConfig
      result <- stripe config $ getCustomers (Just 100) Nothing Nothing 
      result `shouldSatisfy` isRight
    it "Gets customers expandable" $ do
      config <- getConfig
      result <- stripe config $ getCustomersExpandable
                 Nothing Nothing Nothing ["data.default_card"]
      result `shouldSatisfy` isRight
    it "Updates a customer" $ do
      config <- getConfig
      result <- stripe config $ do
        Customer { customerId = cid } <- createEmptyCustomer
        customer <- updateCustomerBase cid
                      bal
                      Nothing
                      Nothing
                      Nothing
                      Nothing
                      Nothing
                      Nothing
                      Nothing
                      desc 
                      email
                      meta
        _ <- deleteCustomer cid
        return customer
      result `shouldSatisfy` isRight
      let Right Customer{..} = result
      customerAccountBalance `shouldBe` (fromJust bal)
      customerDescription `shouldBe` desc
      customerEmail `shouldBe` email
      customerMetaData `shouldBe` meta
  where
    bal = Just 100
    desc = Just "hey"
    email = Just $ Email "djohnson.m@gmail.com"
    meta = [("hey","there")]
    

