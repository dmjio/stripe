{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Test.Stripe.Card (runCardTests) where

import Control.Monad.Trans.Reader
import Control.Monad.IO.Class (liftIO)

import Test.Stripe
import Test.Hspec

import Web.Stripe.Client
import Web.Stripe.Card
import Web.Stripe.Customer

runCardTests :: StripeTest ()
runCardTests = do
  config <- ask
  Right (Customer { customerId = customerId }) <- liftIO (runStripe config createEmptyCustomer)
  cardId <- createCardTest customerId
  getCardTest customerId cardId
  updateCardTest customerId cardId
  deleteCardTest customerId cardId
  retrieveDeletedCardTest customerId cardId
  liftIO $ runStripe config $ deleteCustomer customerId
  liftIO $ putStrLn "done"

createCardTest 
    :: CustomerId
    -> StripeTest CardId
createCardTest
    customerId = do
  config <- ask
  liftIO $ do
    result <- runStripe config (card customerId)
    case result of
      Left err -> error (show err)
      Right Card { cardId = cardId } -> do
        putStrLn "Successfully created Customer Card"
        return cardId
  where
    card customerId = createCustomerCard customerId num exp year cvc
    num  = CardNumber "4242424242424242"
    exp  = ExpMonth 10
    year = ExpYear 2015
    cvc  = CVC "123"

getCardTest 
    :: CustomerId
    -> CardId
    -> StripeTest ()
getCardTest 
    customerId
    cardId = do
  config <- ask
  liftIO $ do
    result <- runStripe config $ getCustomerCard customerId cardId
    case result of
      Left err -> error (show err)
      Right Card { cardId = cardId } -> do
        putStrLn "Successfully retrieved Customer Card"


updateCardTest 
    :: CustomerId
    -> CardId
    -> StripeTest ()
updateCardTest
    customerId
    cardId = do
  config <- ask
  liftIO $ do
    result <- runStripe config $ updateCustomerCard customerId cardId (Just "testname")
                                   Nothing Nothing Nothing
                                   Nothing Nothing Nothing
    case result of
      Left err -> error (show err)
      Right Card { cardId = cardId, cardName = cardName } -> do
        if cardName == Just "testname"
           then do putStrLn "Successfully updated Customer Card"
           else error "Couldn't update Customer Card"

deleteCardTest 
    :: CustomerId
    -> CardId
    -> StripeTest ()
deleteCardTest
    customerId
    cardId = do
  config <- ask
  liftIO $ do
    result <- runStripe config $ deleteCustomerCard customerId cardId
    case result of
      Left err -> error (show err)
      Right _ -> do
           putStrLn "Successfully deleted Customer Card"

retrieveDeletedCardTest 
    :: CustomerId
    -> CardId
    -> StripeTest ()
retrieveDeletedCardTest
    customerId
    cardId = do
  config <- ask
  liftIO $ do
    result <- runStripe config $ getCustomerCard customerId cardId
    case result of
      Left err -> do
            putStrLn "Successfully errored when trying to retrieve a deleted Customer Card"
      Right _ -> do
         error "Retrieved customer card.. did it not get deleted?"

           


