{-# LANGUAGE OverloadedStrings #-}

module Web.Stripe.Invoice
    ( -- * Invoice Types
      Invoice   (..)
    , InvoiceId (..)
     -- * API calls
    , getInvoice
    , createInvoice
    , payInvoice
    , updateInvoice
    , getUpcomingInvoice
    ) where

import           Control.Applicative        ((<$>))
import           Web.Stripe.Client.Internal
import           Web.Stripe.Types

getInvoice
    :: InvoiceId
    -> Stripe Invoice
getInvoice
    (InvoiceId invoiceId) = callAPI request
  where request = StripeRequest GET url params
        url     = "invoices" </> invoiceId
        params  = []

getInvoices
    :: Maybe Limit
    -> Stripe (StripeList Invoice)
getInvoices
    limit = callAPI request
  where request = StripeRequest GET url params
        url     = "invoices"
        params  = getParams [
                   ("limit", toText <$> limit)
                  ]

createInvoice
    :: CustomerId
    -> Stripe Invoice
createInvoice
    (CustomerId customerId) = callAPI request
  where request = StripeRequest POST url params
        url     = "invoices"
        params  = getParams [
                   ("customer", Just customerId)
                  ]

payInvoice
    :: InvoiceId
    -> Stripe Invoice
payInvoice
    (InvoiceId invoiceId) = callAPI request
  where request = StripeRequest POST url params
        url     = "invoices" </> invoiceId </> "pay"
        params  = []

updateInvoice
    :: InvoiceId
    -> Stripe Invoice
updateInvoice
    (InvoiceId invoiceId) = callAPI request
  where request = StripeRequest POST url params
        url     = "invoices" </> invoiceId
        params  = []

getUpcomingInvoice
    :: CustomerId
    -> Stripe Invoice
getUpcomingInvoice
    (CustomerId customerId) = callAPI request
  where request = StripeRequest GET url params
        url     = "invoices" </> "upcoming"
        params  = getParams [
                   ("customer", Just customerId)
                  ]

