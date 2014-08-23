module Web.Stripe.InvoiceItem where  

import Web.Stripe.Types
import Data.Text.Encoding

createInvoiceItem :: CustomerId -> Stripe InvoiceItem
createInvoiceItem (CustomerId customerId) = callAPI request
  where request = StripeRequest POST url params
        url     = "invoiceitems"
        params  = [ ("customer", T.encodeUtf8 customerId)
                  , ("amount", "1000")
                  , ("currency", "usd")
                  ]

getInvoiceItem :: InvoiceItemId -> IO (Either StripeError InvoiceItem)
getInvoiceItem (InvoiceItemId itemId) = callAPI request
  where request = StripeRequest GET url params
        url     = "invoiceitems/" <> itemId
        params  = []

updateInvoiceItem :: InvoiceItemId -> IO (Either StripeError InvoiceItem)
updateInvoiceItem (InvoiceItemId itemId) = callAPI request
  where request = StripeRequest POST url params
        url     = "invoiceitems/" <> itemId
        params  = []

deleteInvoiceItem :: InvoiceItemId -> IO (Either StripeError StripeResult)
deleteInvoiceItem (InvoiceItemId itemId) = callAPI request
  where request = StripeRequest DELETE url params
        url     = "invoiceitems/" <> itemId
        params  = []

