module Web.Stripe.InvoiceItem 
    ( -- * Types 
      InvoiceItemId (..)
    , InvoiceItem   (..)
      -- * API Calls
    , createInvoiceItem
    , getInvoiceItem
    , updateInvoiceItem
    , deleteInvoiceItem
    ) where  

import           Web.Stripe.Client.Internal
import           Web.Stripe.Types

createInvoiceItem
    :: CustomerId
    -> Stripe InvoiceItem
createInvoiceItem
    (CustomerId customerId) = callAPI request
  where request = StripeRequest POST url params
        url     = "invoiceitems"
        params  = getParams [ 
                    ("customer", customerId)
                  , ("amount", "1000")
                  , ("currency", "usd")
                  ]

getInvoiceItem
    :: InvoiceItemId
    -> Stripe InvoiceItem
getInvoiceItem 
    (InvoiceItemId itemId) = callAPI request
  where request = StripeRequest GET url params
        url     = "invoiceitems" </> itemId
        params  = []

updateInvoiceItem
    :: InvoiceItemId
    -> Stripe InvoiceItem
updateInvoiceItem
    (InvoiceItemId itemId) = callAPI request
  where request = StripeRequest POST url params
        url     = "invoiceitems" </> itemId
        params  = []

deleteInvoiceItem
    :: InvoiceItemId
    -> Stripe StripeDeleteResult
deleteInvoiceItem
    (InvoiceItemId itemId) = callAPI request
  where request = StripeRequest DELETE url params
        url     = "invoiceitems" </> itemId
        params  = []

