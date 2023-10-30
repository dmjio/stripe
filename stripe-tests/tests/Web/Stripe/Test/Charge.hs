{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax  #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE RankNTypes #-}
module Web.Stripe.Test.Charge where

import           Data.Either
import           Test.Hspec
import           Web.Stripe.Charge
import           Web.Stripe.Customer
import           Web.Stripe.Test.Prelude
import           Web.Stripe.Card
import           Web.Stripe.PaymentMethod
import           Web.Stripe.StripeRequest (Expandable (Id))
import           Web.Stripe.Token

chargeTests :: StripeSpec
chargeTests stripe =
  describe "Charge tests" $ do
    chargeCustomerTest
    chargeCustomerTest2
    retrieveChargeTest
    updateChargeTest
    retrieveExpandedChargeTest
    retrieveAllChargesTest
    captureChargeTest
  where
    cn  = CardNumber "4242424242424242"
    em  = ExpMonth 12
    ey  = ExpYear 2023
    cvc = CVC "123"
    cardinfo = (mkNewCard cn em ey) { newCardCVC = Just cvc }
    chargeCustomerTest =
      it "Charges a customer succesfully" $ do
        result <- stripe $ do
          Customer { customerId = cid } <- createCustomer -&- cardinfo
          charge <- createCharge (Amount 100) USD -&- cid
          void $ deleteCustomer cid
          return charge
        result `shouldSatisfy` isLeft
    chargeCustomerTest2 =
      it "Charges a customer's card succesfully" $ do
        result <- stripe $ do
          (cid,  PaymentMethod { paymentMethodId = pmid }) <- attachTestPaymentMethod 
          charge <- createCharge (Amount 100) USD -&- cid -&- (Source pmid)
          void $ deleteCustomer cid
          return charge
        result `shouldSatisfy` isRight
    retrieveChargeTest =
      it "Retrieves a charge succesfully" $ do
        result <- stripe $ do
          Customer { customerId = cid } <- createCustomer -&- cardinfo
          Charge { chargeId = chid } <- createCharge (Amount 100) USD -&- cid
          result <- getCharge chid
          void $ deleteCustomer cid
          return result
        result `shouldSatisfy` isRight
    updateChargeTest =
      it "Updates a charge succesfully" $ do
        result <- stripe $ do
          Customer { customerId = cid } <- createCustomer -&- cardinfo
          Charge { chargeId = chid } <- createCharge (Amount 100) USD -&- cid
          _ <- updateCharge chid
                 -&- Description "Cool"
                 -&- MetaData [("hi", "there")]
          result <- getCharge chid
          void $ deleteCustomer cid
          return result
        result `shouldSatisfy` isRight
        let Right Charge { chargeMetaData = cmd, chargeDescription = desc } = result
        cmd `shouldBe` (MetaData [("hi", "there")])
        desc `shouldSatisfy` (==(Just $ Description "Cool"))
    retrieveExpandedChargeTest =
      it "Retrieves an expanded charge succesfully" $ do
        result <- stripe $ do
          Customer { customerId = cid } <- createCustomer -&- cardinfo
          Charge { chargeId = chid } <- createCharge (Amount 100) USD -&- cid
          result <- getCharge chid
                     -&- ExpandParams [ "balance_transaction"
                                      , "customer"
                                      , "invoice"
                                      ]
          void $ deleteCustomer cid
          return result
        result `shouldSatisfy` isRight
    retrieveAllChargesTest =
      it "Retrieves all charges" $ do
        result <- stripe $ do c <- getCharges
                              return c
        result `shouldSatisfy` isRight
    captureChargeTest =
      it "Captures a charge - 2 Step Payment Flow" $ do
        result <- stripe $ do
          Customer { customerId = cid } <- createCustomer -&- cardinfo
          Charge { chargeId = chid } <- createCharge (Amount 100) USD
                                          -&- cid
                                          -&- (Capture False)
          result <- captureCharge chid
          void $ deleteCustomer cid
          return result
        result `shouldSatisfy` isRight
        let Right Charge { chargeCaptured = captured } = result
        captured `shouldBe` True


attachTestPaymentMethod = do 
    Customer { customerId = cid } <- createCustomer
    pm <- createPaymentMethod cardinfo
    pm' <- attachPaymentMethod (paymentMethodId pm) cid
    return (cid, pm')

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
