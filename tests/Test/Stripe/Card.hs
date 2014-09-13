{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Test.Stripe.Card (runCardTests) where

import Control.Monad.Reader

import Test.Stripe

import Web.Stripe.Client
import Web.Stripe.Card
import Web.Stripe.Customer

import Control.Monad.IO.Class (liftIO)

runCardTests :: StripeTest ()
runCardTests = do
  config <- ask
  Right (Customer { customerId = customerId }) <- liftIO (runStripe config createEmptyCustomer)
  cardId <- createCardTest customerId
  getCardTest customerId cardId
  liftIO $ runStripe config $ deleteCustomer customerId
  liftIO $ putStrLn "done"
  
  -- getCardTest    >>= 
  -- getCardsTest   >>= 
  -- deleteCardTest >>=

createCardTest 
    :: CustomerId
    -> StripeTest CardId
createCardTest customerId = do
  config <- ask
  liftIO $ do
    result <- runStripe config (card customerId)
    case result of
      Left err -> error (show err)
      Right Card { cardId = cardId } -> do
        putStrLn "Successfully created Card"
        return cardId
  where
    card customerId = createCustomerCard customerId num exp year cvc
    num  = CardNumber "4242424242424242"
    exp  = ExpMonth 10
    year = ExpYear 2015
    cvc  = CVC "123"

getCardTest :: CustomerId -> CardId -> StripeTest CardId
getCardTest customerId cardId = do
  config <- ask
  liftIO $ do
    result <- runStripe config $ getCustomerCard customerId cardId
    case result of
      Left err -> error (show err)
      Right Card { cardId = cardId } -> do
        putStrLn "Successfully retrieved Customer Card"
        return cardId

