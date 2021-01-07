{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE RankNTypes #-}
module Web.Stripe.Test.SetupIntent where

import           Data.Either   (Either(..), isRight)
import           Test.Hspec
import           Web.Stripe.Test.Util     (makePlanId)
import           Web.Stripe.Test.Prelude

import           Web.Stripe.SetupIntent
import           Web.Stripe.Card
import           Web.Stripe.Customer
import           Web.Stripe.Token

setupIntentTests :: StripeSpec
setupIntentTests stripe = do
  describe "Setup intent tests" $ do
    it "Succesfully creates a SetupIntent" $ do
      result <- stripe $ do
        setupIntent <- createSetupIntent -&- (SetupIntentUsage OffSession)
        void $ cancelSetupIntent (setupIntentId setupIntent)
        return setupIntent
      result `shouldSatisfy` isRight
    it "Successfully updates a SetupIntent" $ do
      result <- stripe $ do
        setupIntent <- createSetupIntent
        Customer { customerId = cid } <- createCustomer
        Token    { tokenId = tkid   } <- createCardToken (Just cardinfo)
        Card { cardId = CardId cardid } <- createCustomerCardByToken cid tkid
        updatedSetupIntent <-
          updateSetupIntent (setupIntentId setupIntent)
            -&- cid
            -&- Description "some description"
            -&- PaymentMethodId cardid
        void $ cancelSetupIntent (setupIntentId setupIntent)
        void $ deleteCustomer cid
        return updatedSetupIntent
      result `shouldSatisfy` isRight
    it "Successfully cancels a SetupIntent" $ do
      result <- stripe $ do
        setupIntent <- createSetupIntent
        cancelledSetupIntent <- cancelSetupIntent (setupIntentId setupIntent)
        return cancelledSetupIntent
      result `shouldSatisfy` isRight
    it "Successfully gets a SetupIntent"$ do
      result <- stripe $ do
        setupIntent <- createSetupIntent
        setupIntent' <- getSetupIntent (setupIntentId setupIntent)
        void $ cancelSetupIntent (setupIntentId setupIntent)
        return setupIntent'
      result `shouldSatisfy` isRight
    it "Successfully confirms a SetupIntent"$ do
      result <- stripe $ do
        Customer { customerId = cid } <- createCustomer
        setupIntent <- createSetupIntent -&- cid
        Token    { tokenId = tkid   } <- createCardToken (Just cardinfo)
        Card { cardId = CardId cardid } <- createCustomerCardByToken cid tkid
        confirmedSetupIntent <-
          confirmSetupIntent (setupIntentId setupIntent) -&- (PaymentMethodId cardid)
        void $ deleteCustomer cid
        return confirmedSetupIntent
      result `shouldSatisfy` isRight
    {- need to do a separate authorization and capture to test this
       https://stripe.com/docs/payments/capture-later
    it "Successfully captures a SetupIntent"$ do
      result <- stripe $ do
        Customer { customerId = cid } <- createCustomer
        setupIntent <- createSetupIntent (Amount 100) USD -&- cid
        Token    { tokenId = tkid   } <- createCardToken (Just cardinfo)
        Card { cardId = CardId cardid } <- createCustomerCardByToken cid tkid
        capturedSetupIntent <- captureSetupIntent (setupIntentId setupIntent)
        void $ deleteCustomer cid
        return capturedSetupIntent
      result `shouldSatisfy` isRight -}
    it "Successfully gets all SetupIntents"$ do
      result <- stripe $ do
        setupIntents <- getSetupIntents
        return setupIntents
      result `shouldSatisfy` isRight

  where
    cardinfo = (mkNewCard credit em ey) { newCardCVC = Just cvc }
    credit = CardNumber "4242424242424242"
    em  = ExpMonth 12
    ey  = ExpYear 2023
    cvc = CVC "123"
