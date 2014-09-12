{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Stripe.Customer ( runCustomerTests ) where

import           Test.Stripe
import           Web.Stripe.Customer
import           Web.Stripe.Client
import           Web.Stripe.Types

import           Control.Monad.Reader

------------------------------------------------------------------------------
-- | Customer Tests
runCustomerTests :: StripeTest ()
runCustomerTests =
  createCustomerTest >>= \cid ->
  listCustomerTest >>
  updateCustomerTest cid >>=
  deleteCustomerTest

listCustomerTest :: StripeTest ()
listCustomerTest = do
  config <- ask
  liftIO $ do
    customer <- runStripe config $ getCustomers Nothing
    case customer of
      Left err -> do
        print err
        putStrLn "Could not get customers! -> fail" >> error "Couldn't create customer"
      Right StripeList{..} ->
        if length list > 1
          then putStrLn "Got Customers Successfully"
          else error "Could not get customers" 

createCustomerTest :: StripeTest CustomerId
createCustomerTest = do
  config <- ask
  liftIO $ do
    customer <- runStripe config createEmptyCustomer
    case customer of
      Left err -> do
        print err
        putStrLn "Could not create customer! -> fail" >> error "Couldn't create customer"
      Right c@Customer{..} ->
        do if customerLiveMode
              then do
                runStripe config $ deleteCustomer customerId
                error errMsg
              else putStrLn "Created Customer Successfully"
           return customerId

updateCustomerTest :: CustomerId -> StripeTest CustomerId
updateCustomerTest custId = do
  config <- ask
  liftIO $ do
    customer <- runStripe config $ updateCustomerAccountBalance custId 1
    case customer of
      Left err -> do
        print err
        putStrLn "Could not update customer! -> fail" >> error "Couldn't update customer"
      Right c@Customer{..} ->
        do putStrLn "Updated Customer Successfully"
           if customerAccountBalance == 1
              then putStrLn "Customer Account Balance Updated Successfully"
              else error "couldn't update customer acccount balance"
           return customerId

deleteCustomerTest
    :: CustomerId
    -> StripeTest ()
deleteCustomerTest
    customerId = do
      config <- ask
      liftIO $ do
        deleteResult <- runStripe config $ deleteCustomer customerId
        case deleteResult of
          Left err -> print err >> error "Couldn't delete customer"
          Right StripeDeleteResult{..} -> do
            if deleted
              then putStrLn "Deleted Customer Successfully"
              else error "Could not delete customer!"


errMsg :: String
errMsg = concat [ "You have entered your production Stripe Key"
                , "please only enter your test key"
                ]




