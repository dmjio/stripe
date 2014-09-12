{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Test.Stripe.Charge (runChargeTests) where

import Control.Monad.Reader

import Test.Stripe

import Web.Stripe.Client
import Web.Stripe.Charge
import Web.Stripe.Customer

runChargeTests :: StripeTest ()
runChargeTests = 
  createChargeTest >>=
  getChargeTest    >>= 
  updateChargeTest

createChargeTest :: StripeTest ChargeId
createChargeTest = do
  config <- ask
  liftIO $ do
    Right Customer { customerId = customerId } <- 
        runStripe config createEmptyCustomer
    result <- runStripe config $ 
                chargeCustomer customerId (Currency "usd") 500 Nothing 
    case result of
      Left err -> error (show err)
      Right c@Charge{..} -> do
        print c
        putStrLn "Successfully created charge"
        return chargeId

getChargeTest :: ChargeId -> StripeTest ChargeId
getChargeTest chargeId = do
  config <- ask
  liftIO $ do
    result <- runStripe config $ getCharge chargeId
    case result of
      Left err -> error (show err)
      Right charge@Charge{..} -> do
        print charge
        putStrLn "Successfully retrieved charge"
        return chargeId

updateChargeTest :: ChargeId -> StripeTest ()
updateChargeTest chargeId = do
  config <- ask
  liftIO $ do
    result <- runStripe config $ updateCharge chargeId "new charge"
    case result of
      Left err -> do
        putStrLn "Could not update charge description"
        error (show err)
      Right charge@Charge{..} -> do
        print charge
        if chargeDescription == Just "new charge"
          then putStrLn "Successfully updated Charge"
          else error "Could not update charge"
      
getCharges :: CustomerId -> StripeTest ()
getCharges customerId = do
  config <- ask
  liftIO $ do
    result <- runStripe config $ getCharges customerId 
    case result of
      Left err -> do
        putStrLn "Couldn't get charges"
        error (show err)
      Right charges -> do
        if length (list charges) > 0
          then putStrLn "Got multiple charges"


    
  

