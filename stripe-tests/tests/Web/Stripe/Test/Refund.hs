{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax  #-}
{-# LANGUAGE RecordWildCards   #-}
module Web.Stripe.Test.Refund where

import           Data.Either
import           Test.Hspec
import           Web.Stripe.Charge
import           Web.Stripe.Customer
import           Web.Stripe.Refund
import           Web.Stripe.Test.Prelude

------------------------------------------------------------------------------
-- | Card Info
cn :: CardNumber
cn  = CardNumber "4242424242424242"

em :: ExpMonth
em  = ExpMonth 12

ey :: ExpYear
ey  = ExpYear 2020

cvc :: CVC
cvc = CVC "123"

cardinfo :: NewCard
cardinfo = (mkNewCard cn em ey) { newCardCVC = Just cvc }

------------------------------------------------------------------------------
-- | Refund Tests
refundTests :: StripeSpec
refundTests stripe = do
    describe "Refund Tests" $ do
      it "Creates a refund succesfully" $ do
        result <- stripe $ do
          Customer { customerId = cid }  <- createCustomer -&- cardinfo
          Charge   { chargeId   = chid } <- createCharge (Amount 100) USD -&- cid
          refund <- createRefund chid
          void $ deleteCustomer cid
          return refund
        result `shouldSatisfy` isRight
      it "Retrieves a refund succesfully" $ do
        result <- stripe $ do
          Customer { customerId = cid  } <- createCustomer -&- cardinfo
          Charge   { chargeId   = chid } <- createCharge (Amount 100) USD -&- cid
          Refund   { refundId   = rid  } <- createRefund chid
          void $ deleteCustomer cid
          void $ getRefund chid rid
        result `shouldSatisfy` isRight
      it "Retrieves a refund succesfully with expansion" $ do
        result <- stripe $ do
          Customer { customerId = cid  } <- createCustomer -&- cardinfo
          Charge   { chargeId   = chid } <- createCharge (Amount 100) USD -&- cid
          Refund   { refundId   = rid  } <- createRefund chid
          r <- getRefund chid rid -&- ExpandParams  ["balance_transaction"]
          void $ deleteCustomer cid
          return r
        result `shouldSatisfy` isRight
      it "Updates a refund succesfully" $ do
        result <- stripe $ do
          Customer { customerId = cid  } <- createCustomer -&- cardinfo
          Charge   { chargeId   = chid } <- createCharge (Amount 100) USD -&- cid
          Refund   { refundId   = rid  } <- createRefund chid
          ref <- updateRefund chid rid -&- (MetaData [("hello","there")])
          void $ deleteCustomer cid
          return ref
        result `shouldSatisfy` isRight
        let Right Refund{..} = result
        refundMetaData `shouldBe` (MetaData [("hello","there")])
      it "Retrieves all refunds for a Charge" $ do
        result <- stripe $ do
          Customer { customerId = cid  } <- createCustomer -&- cardinfo
          Charge   { chargeId   = chid } <- createCharge (Amount 100) USD -&- cid
          Refund { } <- createRefund chid
          r <- getRefunds chid
          void $ deleteCustomer cid
          return r
        result `shouldSatisfy` isRight
        let Right StripeList {..} = result
        length list `shouldBe` 1
      it "Retrieves all refunds for a Charge with expansion" $ do
        result <- stripe $ do
          Customer { customerId = cid  } <- createCustomer -&- cardinfo
          Charge   { chargeId   = chid } <- createCharge (Amount 100) USD -&- cid
          Refund { } <- createRefund chid
          r <- getRefunds chid -&- ExpandParams ["data.balance_transaction"]
          void $ deleteCustomer cid
          return r
        result `shouldSatisfy` isRight
        let Right StripeList {..} = result
        length list `shouldBe` 1
