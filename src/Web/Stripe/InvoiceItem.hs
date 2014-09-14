{-# LANGUAGE OverloadedStrings #-}

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

import           Control.Applicative        ((<$>))
import           Web.Stripe.Client.Internal
import           Web.Stripe.Types

------------------------------------------------------------------------------
-- | Create an invoice for a Customer
createInvoiceItem
    :: CustomerId
    -> Amount
    -> Currency
    -> Maybe InvoiceId
    -> Maybe SubscriptionId
    -> Maybe Description
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
                  , ("amount", toText <$> Just amount)
                  , ("currency", Just currency)
                  , ("invoice", (\(InvoiceId x) -> x) <$> invoiceId)
                  , ("subscription", (\(SubscriptionId x) -> x) <$> subscriptionId)
                  , ("description", description)
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

