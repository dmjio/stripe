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
    -> MetaData   -- ^ `MetaData` of `Customer` to `Invoice`
    -> Stripe Invoice
createInvoice
    (CustomerId customerId)
    metadata    = callAPI request
  where request = StripeRequest POST url params
        url     = "invoices"
        params  = toMetaData metadata ++ getParams [
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
    :: InvoiceId                       -- ^ Get an `Invoice` by `InvoiceId`
    -> Limit                           -- ^ Defaults to 10 if `Nothing` specified
    -> StartingAfter InvoiceLineItemId -- ^ Paginate starting after the following `InvoiceLineItemId`
    -> EndingBefore InvoiceLineItemId  -- ^ Paginate ending before the following `InvoiceLineItemId`
    -> Stripe (StripeList InvoiceLineItem)
getInvoiceLineItems
    (InvoiceId invoiceId)
    limit
    startingAfter
    endingBefore = callAPI request
  where request = StripeRequest GET url params
        url     = "invoices" </> invoiceId </> "lines"
        params  = getParams [
            ("limit", toText `fmap` limit )
          , ("starting_after", (\(InvoiceLineItemId x) -> x) `fmap` startingAfter)
          , ("ending_before", (\(InvoiceLineItemId x) -> x) `fmap` endingBefore)
          ]


------------------------------------------------------------------------------
-- | Retrieve a `StripeList` of `Invoice`s
getInvoices
    :: Maybe Limit                 -- ^ Defaults to 10 if `Nothing` specified
    -> StartingAfter InvoiceItemId -- ^ Paginate starting after the following `Customer`
    -> EndingBefore InvoiceItemId  -- ^ Paginate ending before the following `CustomerID`
    -> Stripe (StripeList Invoice)
getInvoices
    limit
    startingAfter
    endingBefore = callAPI request
  where request = StripeRequest GET url params
        url     = "invoices"
        params  = getParams [
            ("limit", toText `fmap` limit )
          , ("starting_after", (\(InvoiceItemId x) -> x) `fmap` startingAfter)
          , ("ending_before", (\(InvoiceItemId x) -> x) `fmap` endingBefore)
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
    -> MetaData   -- ^ `MetaData` of `Customer` to `Invoice`
    -> Stripe Invoice
updateInvoice
    (InvoiceId invoiceId)
    metadata    = callAPI request
  where request = StripeRequest POST url params
        url     = "invoices" </> invoiceId
        params  = toMetaData metadata

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
