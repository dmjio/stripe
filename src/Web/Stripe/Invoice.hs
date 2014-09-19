{-# LANGUAGE OverloadedStrings #-}
module Web.Stripe.Invoice
    ( -- * API 
      createInvoice
    , getInvoice
    , getInvoiceLineItems
    , getUpcomingInvoice
    , updateInvoice
    , payInvoice
    , getInvoices
       -- * Types
    , Invoice   (..)
    , InvoiceId (..)
    ) where

import           Web.Stripe.Client.Internal
import           Web.Stripe.Types

------------------------------------------------------------------------------
-- | The `Invoice` to be created for a `Customer`
createInvoice
    :: CustomerId -- ^ `CustomerId` of `Customer` to `Invoice`
    -> Stripe Invoice
createInvoice
    (CustomerId customerId) = callAPI request
  where request = StripeRequest POST url params
        url     = "invoices"
        params  = getParams [
                   ("customer", Just customerId)
                  ]

------------------------------------------------------------------------------
-- | Retrieve an `Invoice` by `InvoiceId`
getInvoice
    :: InvoiceId -- ^ Get an `Invoice` by `InvoiceId`
    -> Stripe Invoice
getInvoice
    (InvoiceId invoiceId) = callAPI request
  where request = StripeRequest GET url params
        url     = "invoices" </> invoiceId
        params  = []

------------------------------------------------------------------------------
-- | Retrieve an `Invoice` by `InvoiceId`
getInvoiceLineItems
    :: InvoiceId   -- ^ Get an `Invoice` by `InvoiceId`
    -> Maybe Limit -- ^ The `Limit` on how many `InvoiceLineItems` to return
    -> Stripe (StripeList InvoiceLineItem)
getInvoiceLineItems
    (InvoiceId invoiceId)
    limit = callAPI request
  where request = StripeRequest GET url params
        url     = "invoices" </> invoiceId </> "lines"
        params  = getParams [ 
                   ("limit", fmap toText limit) 
                  ]

------------------------------------------------------------------------------
-- | Retrieve a `StripeList` of `Invoice`s
getInvoices
    :: Maybe Limit -- ^ The `Limit` on the amount of `Invoice`s to return
    -> Stripe (StripeList Invoice)
getInvoices
    limit = callAPI request
  where request = StripeRequest GET url params
        url     = "invoices"
        params  = getParams [
                   ("limit", toText `fmap` limit)
                  ]

------------------------------------------------------------------------------
-- | Pay `Invoice` by `InvoiceId`
payInvoice
    :: InvoiceId -- ^ The `InvoiceId` of the `Invoice` to pay
    -> Stripe Invoice
payInvoice
    (InvoiceId invoiceId) = callAPI request
  where request = StripeRequest POST url params
        url     = "invoices" </> invoiceId </> "pay"
        params  = []

------------------------------------------------------------------------------
-- | Update `Invoice` by `InvoiceId`
updateInvoice
    :: InvoiceId -- ^ The `InvoiceId` of the `Invoice` to update
    -> Stripe Invoice
updateInvoice
    (InvoiceId invoiceId) = callAPI request
  where request = StripeRequest POST url params
        url     = "invoices" </> invoiceId
        params  = []

------------------------------------------------------------------------------
-- | Retrieve an upcoming `Invoice` for a `Customer` by `CustomerId`
getUpcomingInvoice
    :: CustomerId -- ^ The `InvoiceId` of the `Invoice` to retrieve
    -> Stripe Invoice
getUpcomingInvoice
    (CustomerId customerId) = callAPI request
  where request = StripeRequest GET url params
        url     = "invoices" </> "upcoming"
        params  = getParams [
                   ("customer", Just customerId)
                  ]

------------------------------------------------------------------------------
-- | Retrieve a `StripeList` of `Invoice`s
getUpcomingInvoices
    :: CustomerId -- ^ The `InvoiceId` of the `Invoice` to retrieve
    -> Stripe (StripeList Invoice)
getUpcomingInvoices
    (CustomerId customerId) = callAPI request
  where request = StripeRequest GET url params
        url     = "invoices"
        params  = getParams [
                   ("customer", Just customerId)
                  ]
