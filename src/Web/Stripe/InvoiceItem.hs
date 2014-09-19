{-# LANGUAGE OverloadedStrings #-}
module Web.Stripe.InvoiceItem
    ( -- * API 
      createInvoiceItem
    , getInvoiceItem
    , updateInvoiceItem
    , deleteInvoiceItem
      -- * Types
    , InvoiceItemId      (..)
    , InvoiceItem        (..)
    , CustomerId         (..)
    , Currency           (..)
    , InvoiceId          (..)
    , SubscriptionId     (..)
    , Description        (..)
    , StripeDeleteResult (..)
    , Amount
    ) where

import           Web.Stripe.Client.Internal (Method (GET, POST, DELETE), Stripe,
                                             StripeRequest (..), callAPI,
                                             getParams, toText, (</>))
import           Web.Stripe.Types           (Amount, Currency (..),
                                             CustomerId (..), Description (..),
                                             InvoiceId (..), InvoiceItem (..),
                                             InvoiceItemId (..),
                                             StripeDeleteResult (..),
                                             SubscriptionId (..))

------------------------------------------------------------------------------
-- | Create an invoice for a Customer
createInvoiceItem
    :: CustomerId            -- ^ `CustomerId` of `Customer` on which to create an `InvoiceItem`
    -> Amount                -- ^ `Amount` associated with `InvoiceItem`
    -> Currency              -- ^ `Currency` to use for `InvoiceItem`
    -> Maybe InvoiceId       -- ^ `InvoiceId` to use for `InvoiceItem`
    -> Maybe SubscriptionId  -- ^ `SubscriptionId` to use for `InvoiceItem`
    -> Maybe Description     -- ^ `Description` to use for `InvoiceItem`
    -> Stripe InvoiceItem
createInvoiceItem
    (CustomerId customerId)
    amount
    (Currency currency)
    invoiceId
    subscriptionId
    description
        = callAPI request
  where request = StripeRequest POST url params
        url     = "invoiceitems"
        params  = getParams [
                    ("customer", Just customerId)
                  , ("amount", toText `fmap` Just amount)
                  , ("currency", Just currency)
                  , ("invoice", (\(InvoiceId x) -> x) `fmap` invoiceId)
                  , ("subscription", (\(SubscriptionId x) -> x) `fmap` subscriptionId)
                  , ("description", description)
                  ]
------------------------------------------------------------------------------
-- | Retrieve an `InvoiceItem` by `InvoiceItemId`
getInvoiceItem
    :: InvoiceItemId -- ^ `InvoiceItemId` of `InvoiceItem` to retrieve
    -> Stripe InvoiceItem
getInvoiceItem
    (InvoiceItemId itemId) = callAPI request
  where request = StripeRequest GET url params
        url     = "invoiceitems" </> itemId
        params  = []

------------------------------------------------------------------------------
-- | Update an `InvoiceItem` by `InvoiceItemId`
updateInvoiceItem
    :: InvoiceItemId  -- ^ `InvoiceItemId` of `InvoiceItem` to update
    -> Stripe InvoiceItem
updateInvoiceItem
    (InvoiceItemId itemId) = callAPI request
  where request = StripeRequest POST url params
        url     = "invoiceitems" </> itemId
        params  = []

------------------------------------------------------------------------------
-- | Delete an `InvoiceItem` by `InvoiceItemId`
deleteInvoiceItem
    :: InvoiceItemId -- ^ `InvoiceItemdId` of `InvoiceItem` to be deleted
    -> Stripe StripeDeleteResult
deleteInvoiceItem
    (InvoiceItemId itemId) = callAPI request
  where request = StripeRequest DELETE url params
        url     = "invoiceitems" </> itemId
        params  = []

