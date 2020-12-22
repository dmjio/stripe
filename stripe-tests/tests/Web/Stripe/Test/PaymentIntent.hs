{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE RankNTypes #-}
module Web.Stripe.Test.PaymentIntent where

import           Data.Either   (Either(..), isRight)
import           Test.Hspec
import           Web.Stripe.Test.Util     (makePlanId)
import           Web.Stripe.Test.Prelude

import           Web.Stripe.PaymentIntent
import           Web.Stripe.Card
import           Web.Stripe.Customer
import           Web.Stripe.Token

paymentIntentTests :: StripeSpec
paymentIntentTests stripe = do
  describe "Payment intent tests" $ do
    it "Succesfully creates a PaymentIntent" $ do
      result <- stripe $ do
        paymentIntent <- createPaymentIntent (Amount 100) USD
        void $ cancelPaymentIntent (paymentIntentId paymentIntent)
        return paymentIntent
      result `shouldSatisfy` isRight
    it "Successfully updates a PaymentIntent" $ do
      result <- stripe $ do
        paymentIntent <- createPaymentIntent (Amount 100) USD
        updatedPaymentIntent <- updatePaymentIntent (paymentIntentId paymentIntent)
        void $ cancelPaymentIntent (paymentIntentId paymentIntent)
        return updatedPaymentIntent
      result `shouldSatisfy` isRight
    it "Successfully cancels a PaymentIntent" $ do
      result <- stripe $ do
        paymentIntent <- createPaymentIntent (Amount 100) USD
        cancelledPaymentIntent <- cancelPaymentIntent (paymentIntentId paymentIntent)
        return cancelledPaymentIntent
      result `shouldSatisfy` isRight
    it "Successfully gets a PaymentIntent"$ do
      result <- stripe $ do
        paymentIntent <- createPaymentIntent (Amount 100) USD
        paymentIntent' <- getPaymentIntent (paymentIntentId paymentIntent)
        void $ cancelPaymentIntent (paymentIntentId paymentIntent)
        return paymentIntent'
      result `shouldSatisfy` isRight
    it "Successfully confirms a PaymentIntent"$ do
      result <- stripe $ do
        Customer { customerId = cid } <- createCustomer
        paymentIntent <- createPaymentIntent (Amount 100) USD -&- cid
        Token    { tokenId = tkid   } <- createCardToken (Just cardinfo)
        Card { cardId = CardId cardid } <- createCustomerCardByToken cid tkid
        confirmedPaymentIntent <-
          confirmPaymentIntent (paymentIntentId paymentIntent) -&- (PaymentMethodId cardid)
        void $ deleteCustomer cid
        return confirmedPaymentIntent
      result `shouldSatisfy` isRight
    {- need to do a separate authorization and capture to test this
       https://stripe.com/docs/payments/capture-later
    it "Successfully captures a PaymentIntent"$ do
      result <- stripe $ do
        Customer { customerId = cid } <- createCustomer
        paymentIntent <- createPaymentIntent (Amount 100) USD -&- cid
        Token    { tokenId = tkid   } <- createCardToken (Just cardinfo)
        Card { cardId = CardId cardid } <- createCustomerCardByToken cid tkid
        capturedPaymentIntent <- capturePaymentIntent (paymentIntentId paymentIntent)
        void $ deleteCustomer cid
        return capturedPaymentIntent
      result `shouldSatisfy` isRight -}
    it "Successfully gets all PaymentIntents"$ do
      result <- stripe $ do
        paymentIntents <- getPaymentIntents
        return paymentIntents
      result `shouldSatisfy` isRight

  where
    cardinfo = (mkNewCard credit em ey) { newCardCVC = Just cvc }
    debitinfo = (mkNewCard debit em ey) { newCardCVC = Just cvc }
    credit = CardNumber "4242424242424242"
    debit  = CardNumber "4000056655665556"
    em  = ExpMonth 12
    ey  = ExpYear 2020
    cvc = CVC "123"
    country = Country "US"
    routingnumber = RoutingNumber "110000000"
    accountnumber = AccountNumber "000123456789"
    name = Name "David Johnson"
    cardname = Name "cardName"
    cardcity         = AddressCity "Chicago"
    cardcountry      = AddressCountry "US"
    cardaddressOne   = AddressLine1 "123 Fake Street"
    cardaddressTwo   = AddressLine2 "456 Fake Street"
    cardaddressState = AddressState "IL"
    cardzip          = AddressZip "60610"