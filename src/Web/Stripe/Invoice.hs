{-# LANGUAGE OverloadedStrings #-}
module Web.Stripe.Invoice
    ( getInvoice
    , createInvoice
    , payInvoice
    , updateInvoice
    -- , getUpComingInvoice
    ) where

import Web.Stripe.Client.Internal
import Web.Stripe.Types

getInvoice
    :: InvoiceId
    -> Stripe Invoice
getInvoice 
    (InvoiceId invoiceId) = callAPI request
  where request = StripeRequest GET url params
        url     = "invoices" </> invoiceId
        params  = []

getInvoices :: Stripe (StripeList Invoice)
getInvoices = callAPI request
  where request = StripeRequest GET url params
        url     = "invoices"
        params  = []

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
        url     = "invoices/upcoming?customer=" <> customerId
        params  = [] 

