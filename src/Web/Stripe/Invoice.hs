{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Web.Stripe.Invoice
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Web.Stripe.Invoice
    ( -- * API
      createInvoice
    , getInvoice
    , getInvoiceExpandable
    , getInvoices
    , getInvoicesExpandable
    , getInvoiceLineItems
    , getUpcomingInvoice
    , getUpcomingInvoices
    , updateInvoice
    , payInvoice
       -- * Types
    , Invoice             (..)
    , InvoiceId           (..)
    , InvoiceLineItem     (..)
    , InvoiceLineItemId   (..)
    , InvoiceLineItemType (..)
    , Discount            (..)
    , Period              (..)
    ) where

import           Web.Stripe.Client.Internal
import           Web.Stripe.Types
import           Web.Stripe.Types.Util

------------------------------------------------------------------------------
-- | The `Invoice` to be created for a `Customer`
createInvoice
    :: CustomerId -- ^ `CustomerId` of `Customer` to `Invoice`
    -> MetaData   -- ^ `MetaData` of `Customer` to `Invoice`
    -> Stripe Invoice
createInvoice
    customerid
    metadata    = callAPI request
  where request = StripeRequest POST url params
        url     = "invoices"
        params  = toMetaData metadata ++ getParams [
                   ("customer", Just $ getCustomerId customerid)
                  ]

------------------------------------------------------------------------------
-- | Retrieve an `Invoice` by `InvoiceId`
getInvoice
    :: InvoiceId -- ^ Get an `Invoice` by `InvoiceId`
    -> Stripe Invoice
getInvoice
    invoiceid = getInvoiceExpandable invoiceid []

------------------------------------------------------------------------------
-- | Retrieve an `Invoice` by `InvoiceId` with `ExpandParams`
getInvoiceExpandable
    :: InvoiceId    -- ^ Get an `Invoice` by `InvoiceId`
    -> ExpandParams -- ^ `ExpandParams` of the objects for expansion
    -> Stripe Invoice
getInvoiceExpandable
    invoiceid
    expandParams = callAPI request
  where request = StripeRequest GET url params
        url     = "invoices" </> getInvoiceId invoiceid
        params  = toExpandable expandParams

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
    endingBefore =
      getInvoicesExpandable limit startingAfter endingBefore []

------------------------------------------------------------------------------
-- | Retrieve a `StripeList` of `Invoice`s with `ExpandParams`
getInvoicesExpandable
    :: Maybe Limit                 -- ^ Defaults to 10 if `Nothing` specified
    -> StartingAfter InvoiceItemId -- ^ Paginate starting after the following `Customer`
    -> EndingBefore InvoiceItemId  -- ^ Paginate ending before the following `CustomerID`
    -> ExpandParams                -- ^ `ExpandParams` of the objects for expansion
    -> Stripe (StripeList Invoice)
getInvoicesExpandable
    limit
    startingAfter
    endingBefore
    expandParams = callAPI request
  where request = StripeRequest GET url params
        url     = "invoices"
        params  = getParams [
            ("limit", toText `fmap` limit )
          , ("starting_after", (\(InvoiceItemId x) -> x) `fmap` startingAfter)
          , ("ending_before", (\(InvoiceItemId x) -> x) `fmap` endingBefore)
          ] ++ toExpandable expandParams

------------------------------------------------------------------------------
-- | Retrieve an `Invoice` by `InvoiceId`
getInvoiceLineItems
    :: InvoiceId                       -- ^ Get an `Invoice` by `InvoiceId`
    -> Limit                           -- ^ Defaults to 10 if `Nothing` specified
    -> StartingAfter InvoiceLineItemId -- ^ Paginate starting after the following `InvoiceLineItemId`
    -> EndingBefore InvoiceLineItemId  -- ^ Paginate ending before the following `InvoiceLineItemId`
    -> Stripe (StripeList InvoiceLineItem)
getInvoiceLineItems
    invoiceid
    limit
    startingAfter
    endingBefore = callAPI request
  where request = StripeRequest GET url params
        url     = "invoices" </> getInvoiceId invoiceid </> "lines"
        params  = getParams [
            ("limit", toText `fmap` limit )
          , ("starting_after", (\(InvoiceLineItemId x) -> x) `fmap` startingAfter)
          , ("ending_before", (\(InvoiceLineItemId x) -> x) `fmap` endingBefore)
          ]


------------------------------------------------------------------------------
-- | Pay `Invoice` by `InvoiceId`
payInvoice
    :: InvoiceId -- ^ The `InvoiceId` of the `Invoice` to pay
    -> Stripe Invoice
payInvoice
    invoiceid   = callAPI request
  where request = StripeRequest POST url params
        url     = "invoices" </> getInvoiceId invoiceid </> "pay"
        params  = []

------------------------------------------------------------------------------
-- | Update `Invoice` by `InvoiceId`
updateInvoice
    :: InvoiceId -- ^ The `InvoiceId` of the `Invoice` to update
    -> MetaData  -- ^ `MetaData` of `Customer` to `Invoice`
    -> Stripe Invoice
updateInvoice
    invoiceid
    metadata    = callAPI request
  where request = StripeRequest POST url params
        url     = "invoices" </> getInvoiceId invoiceid
        params  = toMetaData metadata

------------------------------------------------------------------------------
-- | Retrieve an upcoming `Invoice` for a `Customer` by `CustomerId`
getUpcomingInvoice
    :: CustomerId -- ^ The `InvoiceId` of the `Invoice` to retrieve
    -> Stripe Invoice
getUpcomingInvoice
    customerid = callAPI request
  where request = StripeRequest GET url params
        url     = "invoices" </> "upcoming"
        params  = getParams [
                   ("customer", Just $ getCustomerId customerid)
                  ]

------------------------------------------------------------------------------
-- | Retrieve a `StripeList` of `Invoice`s
getUpcomingInvoices
    :: CustomerId -- ^ The `InvoiceId` of the `Invoice` to retrieve
    -> Stripe (StripeList Invoice)
getUpcomingInvoices
    customerid = callAPI request
  where request = StripeRequest GET url params
        url     = "invoices"
        params  = getParams [
                   ("customer", Just $ getCustomerId customerid)
                  ]
