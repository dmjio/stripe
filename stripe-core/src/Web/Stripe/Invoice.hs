{-# LANGUAGE OverloadedStrings #-}
-------------------------------------------
-- |
-- Module      : Web.Stripe.Invoice
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- < https:/\/\stripe.com/docs/api#invoices >
--
-- @
-- import Web.Stripe
-- import Web.Stripe.Customer
-- import Web.Stripe.Invoice
-- import Web.Stripe.InvoiceItem
-- import Web.Stripe.Plan
--
-- main :: IO ()
-- main = do
--   let config = SecretKey "secret_key"
--   result <- stripe config $ do
--      Customer { customerId = cid } <- createEmptyCustomer
--      Plan { } <- createPlan (PlanId "planid") 20 USD Day "testplan" []
--      InvoiceItem { } <- createInvoiceItem cid 100 USD Nothing Nothing Nothing []
--      createInvoice cid []
--   case result of
--     Right invoice -> print invoice
--     Left  stripeError -> print stripeError
-- @
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

import           Web.Stripe.StripeRequest (Method(GET, POST), StripeRequest(..),
                                          mkStripeRequest)
import           Web.Stripe.Util  ((</>), getParams, toExpandable,
                                          toMetaData, toText)
import           Web.Stripe.Types        (CustomerId(..), Discount(..), EndingBefore,
                                          ExpandParams, Invoice(..), InvoiceId(..),
                                          InvoiceItemId(..), InvoiceLineItem(..),
                                          InvoiceLineItemId(..), InvoiceLineItemType(..),
                                          Limit, MetaData, Period(..),
                                          StartingAfter, StripeList(..))
import           Web.Stripe.Types.Util   (getCustomerId, getInvoiceId)

------------------------------------------------------------------------------
-- | The `Invoice` to be created for a `Customer`
createInvoice
    :: CustomerId -- ^ `CustomerId` of `Customer` to `Invoice`
    -> MetaData   -- ^ `MetaData` of `Customer` to `Invoice`
    -> StripeRequest Invoice
createInvoice
    customerid
    metadata    = request
  where request = mkStripeRequest POST url params
        url     = "invoices"
        params  = toMetaData metadata ++ getParams [
                   ("customer", Just $ getCustomerId customerid)
                  ]

------------------------------------------------------------------------------
-- | Retrieve an `Invoice` by `InvoiceId`
getInvoice
    :: InvoiceId -- ^ Get an `Invoice` by `InvoiceId`
    -> StripeRequest Invoice
getInvoice
    invoiceid = getInvoiceExpandable invoiceid []

------------------------------------------------------------------------------
-- | Retrieve an `Invoice` by `InvoiceId` with `ExpandParams`
getInvoiceExpandable
    :: InvoiceId    -- ^ Get an `Invoice` by `InvoiceId`
    -> ExpandParams -- ^ `ExpandParams` of the objects for expansion
    -> StripeRequest Invoice
getInvoiceExpandable
    invoiceid
    expandParams = request
  where request = mkStripeRequest GET url params
        url     = "invoices" </> getInvoiceId invoiceid
        params  = toExpandable expandParams

------------------------------------------------------------------------------
-- | Retrieve a `StripeList` of `Invoice`s
getInvoices
    :: Maybe Limit                 -- ^ Defaults to 10 if `Nothing` specified
    -> StartingAfter InvoiceItemId -- ^ Paginate starting after the following `Customer`
    -> EndingBefore InvoiceItemId  -- ^ Paginate ending before the following `CustomerID`
    -> StripeRequest (StripeList Invoice)
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
    -> StripeRequest (StripeList Invoice)
getInvoicesExpandable
    limit
    startingAfter
    endingBefore
    expandParams = request
  where request = mkStripeRequest GET url params
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
    -> StripeRequest (StripeList InvoiceLineItem)
getInvoiceLineItems
    invoiceid
    limit
    startingAfter
    endingBefore = request
  where request = mkStripeRequest GET url params
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
    -> StripeRequest Invoice
payInvoice
    invoiceid   = request
  where request = mkStripeRequest POST url params
        url     = "invoices" </> getInvoiceId invoiceid </> "pay"
        params  = []

------------------------------------------------------------------------------
-- | Update `Invoice` by `InvoiceId`
updateInvoice
    :: InvoiceId -- ^ The `InvoiceId` of the `Invoice` to update
    -> MetaData  -- ^ `MetaData` of `Customer` to `Invoice`
    -> StripeRequest Invoice
updateInvoice
    invoiceid
    metadata    = request
  where request = mkStripeRequest POST url params
        url     = "invoices" </> getInvoiceId invoiceid
        params  = toMetaData metadata

------------------------------------------------------------------------------
-- | Retrieve an upcoming `Invoice` for a `Customer` by `CustomerId`
getUpcomingInvoice
    :: CustomerId -- ^ The `InvoiceId` of the `Invoice` to retrieve
    -> StripeRequest Invoice
getUpcomingInvoice
    customerid = request
  where request = mkStripeRequest GET url params
        url     = "invoices" </> "upcoming"
        params  = getParams [
                   ("customer", Just $ getCustomerId customerid)
                  ]

------------------------------------------------------------------------------
-- | Retrieve a `StripeList` of `Invoice`s
getUpcomingInvoices
    :: CustomerId -- ^ The `InvoiceId` of the `Invoice` to retrieve
    -> StripeRequest (StripeList Invoice)
getUpcomingInvoices
    customerid = request
  where request = mkStripeRequest GET url params
        url     = "invoices"
        params  = getParams [
                   ("customer", Just $ getCustomerId customerid)
                  ]
