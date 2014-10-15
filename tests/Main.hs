{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main where

import           Data.ByteString            (ByteString)
import           Data.Either

import           Web.Stripe
import           Web.Stripe.Customer

import           Test.Hspec

getConfig :: IO StripeConfig
getConfig = return $ StripeConfig secretKey
  where
    secretKey :: ByteString
    secretKey = "sk_test_zvqdM2SSA6WwySqM6KJQrqpH"

main :: IO ()
main = hspec $ customerTests

customerTests :: Spec
customerTests =
  describe "Customer tests" $ do
    it "Creates an empty customer, then deletes it" $ do
      config <- getConfig
      result <- stripe config $ do
        Customer{..} <- createEmptyCustomer
        deleteCustomer customerId
    result `shouldSatisfy` isRight

accountTests :: Spec
accountTests = do
  describe "Customer tests" $ do
    it "Creates an empty customer, then deletes it" $ do
      config <- getConfig
      result <- stripe config $ do
        Customer{..} <- createEmptyCustomer
        deleteCustomer customerId
    result `shouldSatisfy` isRight
  liftIO $ either handleNoAccount handleAccount
      =<< runStripe config getAccountDetails
  where
    handleAccount Account {..} 
        = putStrLn "Retrieved Account Details"
    handleNoAccount
        = error "Couldn't retrieve account details!"


------------------------------------------------------------------------------
-- | Top level testing function
-- tests :: StripeTest ()
-- tests = do
--   runCardTests
--   runAccountTests
--   runCustomerTests
--   runChargeTests


