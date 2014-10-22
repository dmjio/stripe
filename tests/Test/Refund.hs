{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Refund where

import           Control.Monad
import           Data.Either
import           Test.Config         (getConfig)
import           Test.Hspec

import           Web.Stripe
import           Web.Stripe.Charge
import           Web.Stripe.Customer
import           Web.Stripe.Refund

------------------------------------------------------------------------------
-- | Card Info
cn :: CardNumber
cn  = CardNumber "4242424242424242"

em :: ExpMonth
em  = ExpMonth 12

ey :: ExpYear
ey  = ExpYear 2015

cvc :: CVC
cvc = CVC "123"

------------------------------------------------------------------------------
-- | Refund Tests
refundTests :: Spec
refundTests = do
    describe "Refund Tests" $ do
      it "Creates a refund succesfully" $ do
        config <- getConfig
        result <- stripe config $ do
          Customer { customerId = cid }  <- createCustomerByCard cn em ey cvc
          Charge   { chargeId   = chid } <- chargeCustomer cid USD 100 Nothing
          refund <- createRefund chid []
          void $ deleteCustomer cid
          return refund
        result `shouldSatisfy` isRight
      it "Retrieves a refund succesfully" $ do
        config <- getConfig
        result <- stripe config $ do
          Customer { customerId = cid  } <- createCustomerByCard cn em ey cvc
          Charge   { chargeId   = chid } <- chargeCustomer cid USD 100 Nothing
          Refund   { refundId   = rid  } <- createRefund chid []
          void $ deleteCustomer cid
          getRefund chid rid
        result `shouldSatisfy` isRight
      it "Retrieves a refund succesfully with expansion" $ do
        config <- getConfig
        result <- stripe config $ do
          Customer { customerId = cid  } <- createCustomerByCard cn em ey cvc
          Charge   { chargeId   = chid } <- chargeCustomer cid USD 100 Nothing
          Refund   { refundId   = rid  } <- createRefund chid []
          r <- getRefundExpandable chid rid  ["balance_transaction"]
          void $ deleteCustomer cid 
          return r
        result `shouldSatisfy` isRight
      it "Updates a refund succesfully" $ do
        config <- getConfig
        result <- stripe config $ do
          Customer { customerId = cid  } <- createCustomerByCard cn em ey cvc
          Charge   { chargeId   = chid } <- chargeCustomer cid USD 100 Nothing
          Refund   { refundId   = rid  } <- createRefund chid []
          ref <- updateRefund chid rid [("hello","there")]
          void $ deleteCustomer cid
          return ref
        result `shouldSatisfy` isRight
        let Right Refund{..} = result
        refundMetaData `shouldBe` [("hello","there")]
      it "Retrieves all refunds for a Charge" $ do
        config <- getConfig
        result <- stripe config $ do
          Customer { customerId = cid  } <- createCustomerByCard cn em ey cvc
          Charge   { chargeId   = chid } <- chargeCustomer cid USD 100 Nothing
          Refund { } <- createRefund chid []
          r <- getRefunds chid Nothing Nothing Nothing
          void $ deleteCustomer cid
          return r
        result `shouldSatisfy` isRight
        let Right StripeList {..} = result
        length list `shouldBe` 1
      it "Retrieves all refunds for a Charge with expansion" $ do
        config <- getConfig
        result <- stripe config $ do
          Customer { customerId = cid  } <- createCustomerByCard cn em ey cvc
          Charge   { chargeId   = chid } <- chargeCustomer cid USD 100 Nothing
          Refund { } <- createRefund chid []
          r <- getRefundsExpandable chid Nothing Nothing Nothing ["data.balance_transaction"]
          void $ deleteCustomer cid
          return r
        result `shouldSatisfy` isRight
        let Right StripeList {..} = result
        length list `shouldBe` 1





