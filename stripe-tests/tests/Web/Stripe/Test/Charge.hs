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

chargeTests :: StripeSpec
chargeTests stripe =
  describe "Charge tests" $ do
    chargeCustomerTest
    retrieveChargeTest
    updateChargeTest
    retrieveExpandedChargeTest
    retrieveAllChargesTest
    captureChargeTest
  where
    cn  = CardNumber "4242424242424242"
    em  = ExpMonth 12
    ey  = ExpYear 2025
    cvc = CVC "123"
    cardinfo = (mkNewCard cn em ey) { newCardCVC = Just cvc }
    chargeCustomerTest =
      it "Charges a customer succesfully" $ do
        result <- stripe $ do
          Customer { customerId = cid } <- createCustomer -&- cardinfo
          charge <- createCharge (Amount 100) USD -&- cid
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
