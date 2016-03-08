{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax  #-}
{-# LANGUAGE RecordWildCards   #-}
module Web.Stripe.Test.Invoice where

import           Data.Either
import           Test.Hspec
import           Web.Stripe.Customer
import           Web.Stripe.Invoice
import           Web.Stripe.InvoiceItem
import           Web.Stripe.Plan
import           Web.Stripe.Test.Prelude
import           Web.Stripe.Test.Util

invoiceTests :: StripeSpec
invoiceTests stripe = do
  describe "Invoice tests" $ do
    it "Create an Invoice via Invoice item on a Customer" $ do
      result <- stripe $ do
        Customer { customerId = cid } <- createCustomer
        InvoiceItem { invoiceItemId = iiid } <- createInvoiceItem cid (Amount 100) USD
        i <- createInvoice cid -&- meta
        void $ deleteInvoiceItem iiid
        void $ deleteCustomer cid
        return i
      result `shouldSatisfy` isRight
    it "Retrieve an Invoice" $ do
      planid <- makePlanId
      result <- stripe $ do
        Customer { customerId = cid } <- createCustomer
        Plan { planId = pid} <- createPlan planid (Amount 20) USD Day (PlanName "testplan")
        InvoiceItem { invoiceItemId = iiid } <- createInvoiceItem cid (Amount 100) USD
        Invoice { invoiceId = Just iid } <- createInvoice cid -&- meta
        i <- getInvoice iid
        void $ deleteInvoiceItem iiid
        void $ deletePlan pid
        void $ deleteCustomer cid
        return i
      result `shouldSatisfy` isRight
    it "Retrieve an Invoice Expanded" $ do
      planid <- makePlanId
      result <- stripe $ do
        Customer { customerId = cid } <- createCustomer
        Plan { planId = pid} <- createPlan planid (Amount 20) USD Day (PlanName "testplan")
        InvoiceItem { invoiceItemId = iiid } <- createInvoiceItem cid (Amount 100) USD
        Invoice { invoiceId = Just iid } <- createInvoice cid -&- meta
        i <- getInvoice iid -&- ExpandParams ["customer", "charge"]
        void $ deleteInvoiceItem iiid
        void $ deletePlan pid
        void $ deleteCustomer cid
        return i
      result `shouldSatisfy` isRight
    it "Retrieve an Invoice's Line Items" $ do
      planid <- makePlanId
      result <- stripe $ do
        Customer { customerId = cid } <- createCustomer
        Plan { planId = pid } <- createPlan planid (Amount 20) USD Day (PlanName "testplan")
        InvoiceItem { invoiceItemId = iiid } <- createInvoiceItem cid (Amount 100) USD
        Invoice { invoiceId = Just iid } <- createInvoice cid -&- meta
        i <- getInvoiceLineItems iid
        void $ deleteInvoiceItem iiid
        void $ deletePlan pid
        void $ deleteCustomer cid
        return i
      result `shouldSatisfy` isRight
    it "Retrieve Invoices" $ do
      result <- stripe $ void $ getInvoices
      result `shouldSatisfy` isRight
    it "Retrieve Invoices Expandable" $ do
      result <- stripe $ void $ getInvoices
                  -&- ExpandParams ["data.customer", "data.charge"]
      result `shouldSatisfy` isRight
    it "Updates an Invoice" $ do
      planid <- makePlanId
      result <- stripe $ do
        Customer { customerId = cid } <- createCustomer
        Plan { planId = pid } <- createPlan planid (Amount 20) USD Day (PlanName "testplan")
        InvoiceItem { invoiceItemId = iiid } <- createInvoiceItem cid (Amount 100) USD
        Invoice { invoiceId = Just iid } <- createInvoice cid -&- meta
        i <- updateInvoice iid -&- (MetaData [("some", "thing")])
        void $ deleteInvoiceItem iiid
        void $ deletePlan pid
        void $ deleteCustomer cid
        return i
      result `shouldSatisfy` isRight
      let Right Invoice {..} = result
      invoiceMetaData `shouldBe` (MetaData [("some", "thing")])
    it "Retrieve an Upcoming Invoice" $ do
      planid <- makePlanId
      result <- stripe $ do
        Customer { customerId = cid } <- createCustomer
        Plan { planId = pid } <- createPlan planid (Amount 20) USD Day (PlanName "testplan")
        InvoiceItem { invoiceItemId = iiid } <- createInvoiceItem cid (Amount 100) USD
        i <- getUpcomingInvoice cid
        void $ deleteInvoiceItem iiid
        void $ deletePlan pid
        void $ deleteCustomer cid
        return i
      result `shouldSatisfy` isRight
    it "Pay an Invoice" $ do
      planid <- makePlanId
      result <- stripe $ do
        Customer { customerId = cid } <- createCustomer -&- cardinfo
        Plan { planId = pid } <- createPlan planid (Amount 20) USD Day (PlanName "testplan")
        InvoiceItem { } <- createInvoiceItem cid (Amount 100) USD
        Invoice { invoiceId = Just iid } <- createInvoice cid -&- meta
        i <- payInvoice iid
        void $ deletePlan pid
        void $ deleteCustomer cid
        return i
      result `shouldSatisfy` isRight
      let Right Invoice{..} = result
      invoicePaid `shouldBe` True
  where
    cardinfo = (mkNewCard credit em ey) { newCardCVC = Just cvc }
    meta = MetaData [ ("some","metadata") ]
    credit = CardNumber "4242424242424242"
    em  = ExpMonth 12
    ey  = ExpYear 2020
    cvc = CVC "123"
