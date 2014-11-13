{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
module Test.Invoice where

import           Data.Either
import           Test.Hspec
import           Test.Prelude
import           Test.Util
import           Web.Stripe.Invoice
import           Web.Stripe.Plan
import           Web.Stripe.Customer
import           Web.Stripe.InvoiceItem

invoiceTests :: StripeSpec
invoiceTests stripe = do
  describe "Invoice tests" $ do
    it "Create an Invoice via Invoice item on a Customer" $ do
      result <- stripe $ do
        Customer { customerId = cid } <- createEmptyCustomer
        InvoiceItem { } <- createInvoiceItem cid 100 USD Nothing Nothing Nothing []
        i <- createInvoice cid meta
        void $ deleteCustomer cid
        return i
      result `shouldSatisfy` isRight
    it "Retrieve an Invoice" $ do
      planid <- makePlanId
      result <- stripe $ do
        Customer { customerId = cid } <- createEmptyCustomer
        Plan { } <- createPlan planid 20 USD Day "testplan" []
        InvoiceItem { } <- createInvoiceItem cid 100 USD Nothing Nothing Nothing []
        Invoice { invoiceId = Just iid } <- createInvoice cid meta
        i <- getInvoice iid
        return i
      result `shouldSatisfy` isRight
    it "Retrieve an Invoice Expanded" $ do
      planid <- makePlanId
      result <- stripe $ do
        Customer { customerId = cid } <- createEmptyCustomer
        Plan { } <- createPlan planid 20 USD Day "testplan" []
        InvoiceItem { } <- createInvoiceItem cid 100 USD Nothing Nothing Nothing []
        Invoice { invoiceId = Just iid } <- createInvoice cid meta
        i <- getInvoiceExpandable iid ["customer", "charge"]
        return i
      result `shouldSatisfy` isRight
    it "Retrieve an Invoice's Line Items" $ do
      planid <- makePlanId
      result <- stripe $ do
        Customer { customerId = cid } <- createEmptyCustomer
        Plan { } <- createPlan planid 20 USD Day "testplan" []
        InvoiceItem { } <- createInvoiceItem cid 100 USD Nothing Nothing Nothing []
        Invoice { invoiceId = Just iid } <- createInvoice cid meta
        i <- getInvoiceLineItems iid Nothing Nothing Nothing
        return i
      result `shouldSatisfy` isRight
    it "Retrieve Invoices" $ do
      result <- stripe $ void $ getInvoices Nothing Nothing Nothing
      result `shouldSatisfy` isRight
    it "Retrieve Invoices Expandable" $ do
      result <- stripe $ void $ getInvoicesExpandable Nothing Nothing Nothing
                ["data.customer", "data.charge"]
      result `shouldSatisfy` isRight
    it "Updates an Invoice" $ do
      planid <- makePlanId
      result <- stripe $ do
        Customer { customerId = cid } <- createEmptyCustomer
        Plan { } <- createPlan planid 20 USD Day "testplan" []
        InvoiceItem { } <- createInvoiceItem cid 100 USD Nothing Nothing Nothing []
        Invoice { invoiceId = Just iid } <- createInvoice cid meta
        i <- updateInvoice iid [("some", "thing")]
        return i
      result `shouldSatisfy` isRight
      let Right Invoice {..} = result
      invoiceMetaData `shouldBe` [("some", "thing")]
    it "Retrieve an Upcoming Invoice" $ do
      planid <- makePlanId
      result <- stripe $ do
        Customer { customerId = cid } <- createEmptyCustomer
        Plan { } <- createPlan planid 20 USD Day "testplan" []
        InvoiceItem { } <- createInvoiceItem cid 100 USD Nothing Nothing Nothing []
        i <- getUpcomingInvoice cid
        return i
      result `shouldSatisfy` isRight
    it "Pay an Invoice" $ do
      planid <- makePlanId
      result <- stripe $ do
        Customer { customerId = cid } <- createCustomerByCard credit em ey cvc
        Plan { } <- createPlan planid 20 USD Day "testplan" []
        InvoiceItem { } <- createInvoiceItem cid 100 USD Nothing Nothing Nothing []
        Invoice { invoiceId = Just iid } <- createInvoice cid meta
        i <- payInvoice iid
        return i
      result `shouldSatisfy` isRight
      let Right Invoice{..} = result
      invoicePaid `shouldBe` True
  where
    meta = [ ("some","metadata") ]
    credit = CardNumber "4242424242424242"
    em  = ExpMonth 12
    ey  = ExpYear 2015
    cvc = CVC "123"
