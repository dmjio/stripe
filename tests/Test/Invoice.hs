{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Invoice where

import           Data.Either

import           Test.Config            (getConfig)
import           Control.Monad
import           Test.Util        
import           Test.Hspec

import           Web.Stripe
import           Web.Stripe.Invoice
import           Web.Stripe.Plan
import           Web.Stripe.Customer
import           Web.Stripe.InvoiceItem

invoiceTests :: Spec
invoiceTests = do
  describe "Invoice tests" $ do
    it "Create an invoice an customer from a line item" $ do
      config <- getConfig
      planid <- makePlanId
      result <- stripe config $ do
        Customer { customerId = cid } <- createEmptyCustomer
        Plan { } <- createPlan planid 20 USD Day "testplan" []
        InvoiceItem { } <- createInvoiceItem cid 100 USD Nothing Nothing Nothing []
        i <- createInvoice cid meta 
        void $ deleteCustomer cid
        void $ deletePlan planid
        return i
      result `shouldSatisfy` isRight
    it "Retrieve an Invoice" $ do
      config <- getConfig
      planid <- makePlanId
      result <- stripe config $ do
        Customer { customerId = cid } <- createEmptyCustomer
        Plan { } <- createPlan planid 20 USD Day "testplan" []
        InvoiceItem { } <- createInvoiceItem cid 100 USD Nothing Nothing Nothing []
        Invoice { invoiceId = Just iid } <- createInvoice cid meta 
        getInvoice iid
      result `shouldSatisfy` isRight
    it "Retrieve an Invoice Expanded" $ do
      config <- getConfig
      planid <- makePlanId
      result <- stripe config $ do
        Customer { customerId = cid } <- createEmptyCustomer
        Plan { } <- createPlan planid 20 USD Day "testplan" []
        InvoiceItem { } <- createInvoiceItem cid 100 USD Nothing Nothing Nothing []
        Invoice { invoiceId = Just iid } <- createInvoice cid meta 
        getInvoiceExpandable iid ["customer", "charge"]
      result `shouldSatisfy` isRight
    it "Retrieve an Invoice's Line Items" $ do
      config <- getConfig
      planid <- makePlanId
      result <- stripe config $ do
        Customer { customerId = cid } <- createEmptyCustomer
        Plan { } <- createPlan planid 20 USD Day "testplan" []
        InvoiceItem { } <- createInvoiceItem cid 100 USD Nothing Nothing Nothing []
        Invoice { invoiceId = Just iid } <- createInvoice cid meta 
        getInvoiceLineItems iid Nothing Nothing Nothing
      result `shouldSatisfy` isRight
    it "Retrieve Invoices" $ do
      config <- getConfig
      result <- stripe config $ getInvoices Nothing Nothing Nothing 
      result `shouldSatisfy` isRight
    it "Retrieve Invoices Expandable" $ do
      config <- getConfig
      result <- stripe config $ getInvoicesExpandable Nothing Nothing Nothing
                ["data.customer", "data.charge"]
      result `shouldSatisfy` isRight
    it "Updates an Invoice" $ do
      config <- getConfig
      planid <- makePlanId
      result <- stripe config $ do
        Customer { customerId = cid } <- createEmptyCustomer
        Plan { } <- createPlan planid 20 USD Day "testplan" []
        InvoiceItem { } <- createInvoiceItem cid 100 USD Nothing Nothing Nothing []
        Invoice { invoiceId = Just iid } <- createInvoice cid meta 
        updateInvoice iid [("some", "thing")]
      result `shouldSatisfy` isRight
      let Right Invoice {..} = result
      invoiceMetaData `shouldBe` [("some", "thing")]
    it "Retrieve an Upcoming Invoice" $ do
      config <- getConfig
      planid <- makePlanId
      result <- stripe config $ do
        Customer { customerId = cid } <- createEmptyCustomer
        Plan { } <- createPlan planid 20 USD Day "testplan" []
        InvoiceItem { } <- createInvoiceItem cid 100 USD Nothing Nothing Nothing []
        getUpcomingInvoice cid
      result `shouldSatisfy` isRight
    it "Pay an Invoice" $ do
      config <- getConfig
      planid <- makePlanId
      result <- stripe config $ do
        Customer { customerId = cid } <- createCustomerByCard credit em ey cvc
        Plan { } <- createPlan planid 20 USD Day "testplan" []
        InvoiceItem { } <- createInvoiceItem cid 100 USD Nothing Nothing Nothing []
        Invoice { invoiceId = Just iid } <- createInvoice cid meta 
        payInvoice iid
      result `shouldSatisfy` isRight
      let Right Invoice{..} = result 
      invoicePaid `shouldBe` True
  where
    meta = [ ("some","metadata") ]
    credit = CardNumber "4242424242424242"
    em  = ExpMonth 12
    ey  = ExpYear 2015
    cvc = CVC "123"

