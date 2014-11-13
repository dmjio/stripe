{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
module Test.InvoiceItem where

import           Data.Either

import           Test.Hspec
import           Test.Prelude

import           Web.Stripe.InvoiceItem
import           Web.Stripe.Customer

invoiceItemTests :: StripeSpec
invoiceItemTests stripe = do
  describe "Invoice item tests" $ do
    it "Succesfully creates an invoice item" $ do
      result <- stripe $ do
        Customer { customerId = cid } <- createEmptyCustomer
        ii <- createInvoiceItem cid 100 USD Nothing Nothing (Just "hey") []
        _ <- deleteCustomer cid
        return ii
      result `shouldSatisfy` isRight
    it "Succesfully retrieves an existing invoice item" $ do
      result <- stripe $ do
        Customer { customerId = cid } <- createEmptyCustomer
        InvoiceItem { invoiceItemId = iid } <-
           createInvoiceItem cid 100 USD Nothing Nothing (Just "hey") []
        ii <- getInvoiceItem iid
        _ <- deleteCustomer cid
        return ii
      result `shouldSatisfy` isRight
    it "Succesfully retrieves an existing invoice item expandable" $ do
      result <- stripe $ do
        Customer { customerId = cid } <- createEmptyCustomer
        InvoiceItem { invoiceItemId = iid } <-
           createInvoiceItem cid 100 USD Nothing Nothing (Just "hey") []
        ii <- getInvoiceItemExpandable iid ["customer"]
        _ <- deleteCustomer cid
        return ii
      result `shouldSatisfy` isRight
    it "Succesfully retrieves invoice items" $ do
      result <- stripe $ do
        Customer { customerId = cid } <- createEmptyCustomer
        InvoiceItem {  } <-
           createInvoiceItem cid 100 USD Nothing Nothing (Just "hey") []
        ii <- getInvoiceItems Nothing Nothing Nothing Nothing
        _ <- deleteCustomer cid
        return ii
      result `shouldSatisfy` isRight
    it "Succesfully retrieves invoice items with expansion" $ do
      result <- stripe $ do
        Customer { customerId = cid } <- createEmptyCustomer
        InvoiceItem {  } <-
           createInvoiceItem cid 100 USD Nothing Nothing (Just "hey") []
        ii <- getInvoiceItemsExpandable Nothing Nothing Nothing Nothing ["data.customer"]
        _ <- deleteCustomer cid
        return ii
      result `shouldSatisfy` isRight
    it "Succesfully updates an existing invoice item" $ do
      result <- stripe $ do
        Customer { customerId = cid } <- createEmptyCustomer
        InvoiceItem { invoiceItemId = iid } <-
           createInvoiceItem cid 100 USD Nothing Nothing (Just "hey") []
        ii <- updateInvoiceItem iid (Just 200) (Just "description") [("some","thing")]
        _  <- deleteCustomer cid
        return ii
      result `shouldSatisfy` isRight
      let Right InvoiceItem{..} = result
      invoiceItemMetaData `shouldBe` [("some","thing")]
      invoiceItemDescription `shouldBe` Just "description"
      invoiceItemAmount `shouldBe` 200
    it "Succesfully deletes an invoice item" $ do
      result <- stripe $ do
        Customer { customerId = cid } <- createEmptyCustomer
        InvoiceItem { invoiceItemId = iid } <-
           createInvoiceItem cid 100 USD Nothing Nothing (Just "hey") []
        result <- deleteInvoiceItem iid
        _ <- deleteCustomer cid
        return result
      result `shouldSatisfy` isRight
