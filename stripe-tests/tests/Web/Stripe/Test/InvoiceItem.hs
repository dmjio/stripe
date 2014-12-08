{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
module Web.Stripe.Test.InvoiceItem where

import           Data.Either

import           Test.Hspec
import           Web.Stripe.Test.Prelude

import           Web.Stripe.InvoiceItem
import           Web.Stripe.Customer

invoiceItemTests :: StripeSpec
invoiceItemTests stripe = do
  describe "Invoice item tests" $ do
    it "Succesfully creates an invoice item" $ do
      result <- stripe $ do
        Customer { customerId = cid } <- createCustomer
        ii <- createInvoiceItem cid (Amount 100) USD
              -&- (Description "hey")
        _ <- deleteCustomer cid
        return ii
      result `shouldSatisfy` isRight
    it "Succesfully retrieves an existing invoice item" $ do
      result <- stripe $ do
        Customer { customerId = cid } <- createCustomer
        InvoiceItem { invoiceItemId = iid } <-
           createInvoiceItem cid (Amount 100) USD -&- (Description "hey")
        ii <- getInvoiceItem iid
        _ <- deleteCustomer cid
        return ii
      result `shouldSatisfy` isRight
    it "Succesfully retrieves an existing invoice item expandable" $ do
      result <- stripe $ do
        Customer { customerId = cid } <- createCustomer
        InvoiceItem { invoiceItemId = iid } <-
           createInvoiceItem cid (Amount 100) USD -&- (Description "hey")
        ii <- getInvoiceItem iid -&- ExpandParams ["customer"]
        _ <- deleteCustomer cid
        return ii
      result `shouldSatisfy` isRight
    it "Succesfully retrieves invoice items" $ do
      result <- stripe $ do
        Customer { customerId = cid } <- createCustomer
        InvoiceItem {  } <-
           createInvoiceItem cid (Amount 100) USD -&- (Description "hey")
        ii <- getInvoiceItems
        _ <- deleteCustomer cid
        return ii
      result `shouldSatisfy` isRight
    it "Succesfully retrieves invoice items with expansion" $ do
      result <- stripe $ do
        Customer { customerId = cid } <- createCustomer
        InvoiceItem {  } <-
           createInvoiceItem cid (Amount 100) USD -&- (Description "hey")
        ii <- getInvoiceItems -&- ExpandParams ["data.customer"]
        _ <- deleteCustomer cid
        return ii
      result `shouldSatisfy` isRight
    it "Succesfully updates an existing invoice item" $ do
      result <- stripe $ do
        Customer { customerId = cid } <- createCustomer
        InvoiceItem { invoiceItemId = iid } <-
           createInvoiceItem cid (Amount 100) USD
              -&- (Description "hey")
        ii <- updateInvoiceItem iid
              -&- (Amount 200)
              -&- (Description "description")
              -&- MetaData [("some","thing")]
        _  <- deleteCustomer cid
        return ii
      result `shouldSatisfy` isRight
      let Right InvoiceItem{..} = result
      invoiceItemMetaData `shouldBe` (MetaData [("some","thing")])
      invoiceItemDescription `shouldBe` (Just (Description "description"))
      invoiceItemAmount `shouldBe` 200
    it "Succesfully deletes an invoice item" $ do
      result <- stripe $ do
        Customer { customerId = cid } <- createCustomer
        InvoiceItem { invoiceItemId = iid } <-
           createInvoiceItem cid (Amount 100) USD
             -&- (Description "hey")
        result <- deleteInvoiceItem iid
        _ <- deleteCustomer cid
        return result
      result `shouldSatisfy` isRight
