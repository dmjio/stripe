module Web.Stripe.Invoice
    ( getInvoice
    , createInvoice
    , payInvoice
    , updateInvoice
    , getUpComingInvoice
    ) where

import Web.Stripe.Types

getInvoice :: InvoiceId -> Stripe Invoice
getInvoice (InvoiceId invoiceId) = callAPI request
  where request = StripeRequest GET url params
        url     = "invoices/" <> invoiceId
        params  = []

createInvoice :: CustomerId -> Stripe Invoice
createInvoice (CustomerId customerId) = callAPI request
  where request = StripeRequest POST url params
        url     = "invoices"
        params  = [ ("customer", T.encodeUtf8 customerId) ]

payInvoice :: InvoiceId -> Stripe Invoice
payInvoice (InvoiceId invoiceId) = callAPI request
  where request = StripeRequest POST url params
        url     = "invoices/" <> invoiceId <> "/pay"
        params  = []

updateInvoice :: InvoiceId -> Stripe Invoice
updateInvoice (InvoiceId invoiceId) = callAPI req 
  where request = StripeRequest POST url params
        url     = "invoices/" <> invoiceId
        params  = []

getUpcomingInvoice :: CustomerId -> Stripe Invoice
getUpcomingInvoice (CustomerId customerId) = callAPI req
  where request = StripeRequest GET url params
        url     = "invoices/upcoming?customer=" <> customerId
        params  = [] 

