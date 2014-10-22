{-# LANGUAGE OverloadedStrings #-}
module Test.Invoice where

import           Data.Either
import           Test.Config        (getConfig)
import           Test.Hspec
import           Web.Stripe
import           Web.Stripe.Invoice
import           Web.Stripe.Customer


invoiceTests :: Spec
invoiceTests = do
  describe "Invoice tests" $ do
    it "Cannot invoice an empty customer" $ do
      config <- getConfig
      result <- stripe config $ do
        Customer { customerId = cid } <- createEmptyCustomer
        createInvoice cid meta
      result `shouldSatisfy` isLeft
    -- it "Successfully creates an invoice" $ do
    --   config <- getConfig
    --   result <- stripe config $ do
    --     Customer { customerId = cid } <- createEmptyCustomer
    --     createInvoice cid meta
    --   result `shouldSatisfy` isLeft


  where meta = [ ("some","metadata") ]
