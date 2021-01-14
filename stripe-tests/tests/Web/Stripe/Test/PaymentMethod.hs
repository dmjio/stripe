{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax  #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE RankNTypes #-}
module Web.Stripe.Test.PaymentMethod where

import           Data.Either
import           Data.Maybe
import           Test.Hspec
import           Web.Stripe.Card
import           Web.Stripe.PaymentMethod
import           Web.Stripe.Customer
import           Web.Stripe.StripeRequest (Expandable (Id))
import           Web.Stripe.Test.Prelude
import           Web.Stripe.Token

paymentMethodTests :: StripeSpec
paymentMethodTests stripe = do
    describe "Card tests" $ do
      it "can create a payment method by CardNumber" $ do
        result <- stripe $ do
          pm <- createPaymentMethod cardinfo
          return pm
        result `shouldSatisfy` isRight

      it "can create a payment method by TokenId" $ do
        result <- stripe $ do
          Token    { tokenId = tkid   } <- createCardToken (Just cardinfo)
          pm <- createPaymentMethodByToken tkid (PaymentMethodTypeCard)
          return pm
        result `shouldSatisfy` isRight

      it "can attach a card to a customer" $ do
        result <- stripe $ do
          Customer { customerId = cid } <- createCustomer
          pm <- createPaymentMethod cardinfo
          pm' <- attachPaymentMethod (paymentMethodId pm) cid
          void $ deleteCustomer cid
          return pm'
        result `shouldSatisfy` isRight

      it "Can retrieve a payment method" $ do
        result <- stripe $ do
          PaymentMethod { paymentMethodId = pmid }<- createPaymentMethod cardinfo
          pm <- getPaymentMethod pmid
          return pm
        result `shouldSatisfy` isRight
        let Right pm = result
        cardHashLastFour <$> paymentMethodCard pm `shouldBe` Just "4242"
        cardHashExpMonth <$> paymentMethodCard pm `shouldBe` Just em
        cardHashExpYear <$> paymentMethodCard pm `shouldBe` Just ey

      it "Can retrieve a Customer's Cards" $ do
        result <- stripe $ do
          Customer { customerId = customerid
                   } <- createCustomer
          pm <- createPaymentMethod cardinfo
          pm' <- attachPaymentMethod (paymentMethodId pm) customerid
          pms <- getCustomerPaymentMethods customerid
          void $ deleteCustomer customerid
          return pms
        result `shouldSatisfy` isRight

{-
      it "Can retrieve a Customer's Card with expansion" $ do
        result <- stripe $ do
          Customer { customerId = customerid
                   , customerCards = StripeList { list = [ Card { cardId = cardid } ] }
                   } <- createCustomer -&- cardinfo
          card <- getCustomerCard customerid cardid -&- ExpandParams ["customer"]
          void $ deleteCustomer customerid
          return card
        result `shouldSatisfy` isRight
        let Right Card{..} = result
        cardLastFour `shouldBe` "4242"
        cardExpMonth `shouldBe` em
        cardExpYear `shouldBe` ey
-}

      it "Can detach a Customer's Cards" $ do
        result <- stripe $ do
          Customer { customerId = customerid } <- createCustomer
          PaymentMethod { paymentMethodId = pmid }<- createPaymentMethod cardinfo
          pm <- attachPaymentMethod pmid customerid
          result <- detachPaymentMethod pmid
          void $ deleteCustomer customerid
          return result
        result `shouldSatisfy` isRight
{-
      it "Can update a Customer's Card" $ do
        result <- stripe $ do
          Customer { customerId = customerid
                    , customerDefaultCard = Just (Id cardid)
                   } <- createCustomer -&- cardinfo
          result <- updateCustomerCard customerid cardid
                       -&- cardname
                       -&- cardcity
                       -&- cardcountry
                       -&- cardaddressOne
                       -&- cardaddressTwo
                       -&- cardaddressState
                       -&- cardzip
          void $ deleteCustomer customerid
          return result
        result `shouldSatisfy` isRight
        let Right Card{..} = result
        cardName           `shouldBe` (Just cardname)
        cardAddressCity    `shouldBe` (Just cardcity)
        cardAddressCountry `shouldBe` (Just cardcountry)
        cardAddressLine1   `shouldBe` (Just cardaddressOne)
        cardAddressLine2   `shouldBe` (Just cardaddressTwo)
        cardAddressState   `shouldBe` (Just cardaddressState)
        cardAddressZip     `shouldBe` (Just cardzip) -}

  where
    cardinfo = (mkNewCard credit em ey) { newCardCVC = Just cvc }
    debitinfo = (mkNewCard debit em ey) { newCardCVC = Just cvc }
    credit = CardNumber "4242424242424242"
    debit  = CardNumber "4000056655665556"
    em  = ExpMonth 12
    ey  = ExpYear 2023
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
