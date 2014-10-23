{-# LANGUAGE OverloadedStrings #-}
module Test.Invoice where

import           Data.Either
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad

import           Test.Config        (getConfig)
import           Test.Util        
import           Test.Hspec

import           Web.Stripe
import           Web.Stripe.Invoice
import           Web.Stripe.Plan
import           Web.Stripe.Customer
import           Web.Stripe.InvoiceItem
import           Web.Stripe.Subscription

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
        createInvoice cid meta 
      result `shouldSatisfy` isRight
    it "Retrieve an Invoice" $ do
      config <- getConfig
      planid <- makePlanId
      result <- stripe config $ do
        Customer { customerId = cid } <- createEmptyCustomer
        Plan { } <- createPlan planid 20 USD Day "testplan" []
        InvoiceItem { } <- createInvoiceItem cid 100 USD Nothing Nothing Nothing []
        Invoice { invoiceId = iid } <- createInvoice cid meta 
        getInvoice iid
      result `shouldSatisfy` isRight
    it "Retrieve an Invoice Expanded" $ do
      config <- getConfig
      planid <- makePlanId
      result <- stripe config $ do
        Customer { customerId = cid } <- createEmptyCustomer
        Plan { } <- createPlan planid 20 USD Day "testplan" []
        InvoiceItem { } <- createInvoiceItem cid 100 USD Nothing Nothing Nothing []
        Invoice { invoiceId = iid } <- createInvoice cid meta 
        getInvoiceExpandable iid ["customer", "charge"]
      result `shouldSatisfy` isRight
    it "Retrieve an Invoice's Line Items" $ do
      config <- getConfig
      planid <- makePlanId
      result <- stripe config $ do
        Customer { customerId = cid } <- createEmptyCustomer
        Plan { } <- createPlan planid 20 USD Day "testplan" []
        InvoiceItem { } <- createInvoiceItem cid 100 USD Nothing Nothing Nothing []
        Invoice { invoiceId = iid } <- createInvoice cid meta 
        getInvoiceLineItems iid Nothing Nothing Nothing
      result `shouldSatisfy` isRight
    -- it "Retrieve an Upcoming Invoice" $ do
    --   config <- getConfig
    --   planid <- makePlanId
    --   result <- stripe config $ do
    --     Customer { customerId = cid } <- createEmptyCustomer
    --     Plan { } <- createPlan planid 20 USD Day "testplan" []
    --     InvoiceItem { } <- createInvoiceItem cid 100 USD Nothing Nothing Nothing []
    --     getUpcomingInvoice cid
    --   print result
    --   result `shouldSatisfy` isRight


  where meta = [ ("some","metadata") ]
