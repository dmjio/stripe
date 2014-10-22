{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Web.Stripe.InvoiceItem
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Web.Stripe.InvoiceItem
    ( -- * API 
      createInvoiceItem
    , getInvoiceItem
    , getInvoiceItemExpandable
    , getInvoiceItems
    , getInvoiceItemsExpandable
    , updateInvoiceItem
    , deleteInvoiceItem
      -- * Types
    , InvoiceItemId      (..)
    , InvoiceItem        (..)
    , CustomerId         (..)
    , Currency           (..)
    , InvoiceId          (..)
    , Invoice            (..)
    , SubscriptionId     (..)
    , StripeDeleteResult (..)
    , StripeList         (..)
    , Description      
    , Amount
    ) where

import           Web.Stripe.Client.Internal (Method (GET, POST, DELETE), Stripe,
                                             StripeRequest (..), callAPI, toMetaData, toExpandable,
                                             getParams, toText, (</>))
import           Web.Stripe.Types           (Amount, Currency (..), StripeList(..),
                                             CustomerId (..), Description,
                                             InvoiceId (..), InvoiceItem (..), Invoice(..),
                                             InvoiceItemId (..), Limit, StartingAfter, EndingBefore,
                                             StripeDeleteResult (..), ExpandParams,
                                             SubscriptionId (..), MetaData)
import           Web.Stripe.Types.Util

------------------------------------------------------------------------------
-- | Create an invoice for a Customer
createInvoiceItem
    :: CustomerId            -- ^ `CustomerId` of `Customer` on which to create an `InvoiceItem`
    -> Amount                -- ^ `Amount` associated with `InvoiceItem`
    -> Currency              -- ^ `Currency` to use for `InvoiceItem`
    -> Maybe InvoiceId       -- ^ `InvoiceId` to use for `InvoiceItem`
    -> Maybe SubscriptionId  -- ^ `SubscriptionId` to use for `InvoiceItem`
    -> Maybe Description     -- ^ `Description` to use for `InvoiceItem`
    -> MetaData              -- ^ `MetaData` to use for `InvoiceItem`
    -> Stripe InvoiceItem
createInvoiceItem
    customerid
    amount
    (Currency currency)
    invoiceid
    subscriptionId
    description
    metadata    = callAPI request
  where request = StripeRequest POST url params
        url     = "invoiceitems"
        params  = toMetaData metadata ++ getParams [
                    ("customer", Just $ getCustomerId customerid)
                  , ("amount", toText `fmap` Just amount)
                  , ("currency", Just currency)
                  , ("invoice", (\(InvoiceId x) -> x) `fmap` invoiceid)
                  , ("subscription", (\(SubscriptionId x) -> x) `fmap` subscriptionId)
                  , ("description", description)
                  ]

------------------------------------------------------------------------------
-- | Retrieve an `InvoiceItem` by `InvoiceItemId`
getInvoiceItems
    :: Maybe CustomerId            -- ^ When specified, only `InvoiceItems` for this `Customer` are returned
    -> Limit                       -- ^ Defaults to 10 if `Nothing` specified
    -> StartingAfter InvoiceItemId -- ^ Paginate starting after the following `InvoiceItemId`
    -> EndingBefore InvoiceItemId  -- ^ Paginate ending before the following `InvoiceItemId`
    -> Stripe (StripeList InvoiceItem)
getInvoiceItems
    customerid
    limit
    startingAfter
    endingBefore = callAPI request
  where request = StripeRequest GET url params
        url     = "invoiceitems"
        params  = getParams [
            ("limit", toText `fmap` limit )
          , ("starting_after", (\(InvoiceItemId x) -> x) `fmap` startingAfter)
          , ("ending_before", (\(InvoiceItemId x) -> x) `fmap` endingBefore)
          , ("customer", (\(CustomerId x) -> x) `fmap` customerid)
          ]

------------------------------------------------------------------------------
-- | Retrieve an `InvoiceItem` by `InvoiceItemId` with `ExpandParams`
getInvoiceItemsExpandable
    :: Maybe CustomerId            -- ^ When specified, only `InvoiceItems` for this `Customer` are returned
    -> Limit                       -- ^ Defaults to 10 if `Nothing` specified
    -> StartingAfter InvoiceItemId -- ^ Paginate starting after the following `InvoiceItemId`
    -> EndingBefore InvoiceItemId  -- ^ Paginate ending before the following `InvoiceItemId`
    -> ExpandParams
    -> Stripe (StripeList InvoiceItem)
getInvoiceItemsExpandable
    customerid
    limit
    startingAfter
    endingBefore
    expandParams = callAPI request
  where request = StripeRequest GET url params
        url     = "invoiceitems"
        params  = getParams [
            ("limit", toText `fmap` limit )
          , ("starting_after", (\(InvoiceItemId x) -> x) `fmap` startingAfter)
          , ("ending_before", (\(InvoiceItemId x) -> x) `fmap` endingBefore)
          , ("customer", (\(CustomerId x) -> x) `fmap` customerid)
--          , ("created", "")
          ] ++ toExpandable expandParams

------------------------------------------------------------------------------
-- | Retrieve an `InvoiceItem` by `InvoiceItemId`
getInvoiceItem
    :: InvoiceItemId -- ^ `InvoiceItemId` of `InvoiceItem` to retrieve
    -> Stripe InvoiceItem
getInvoiceItem
    invoiceitemid =
      getInvoiceItemExpandable invoiceitemid []

------------------------------------------------------------------------------
-- | Retrieve an `InvoiceItem` by `InvoiceItemId`
getInvoiceItemExpandable
    :: InvoiceItemId -- ^ `InvoiceItemId` of `InvoiceItem` to retrieve
    -> ExpandParams
    -> Stripe InvoiceItem
getInvoiceItemExpandable
    invoiceitemid
    expandParams = callAPI request
  where request = StripeRequest GET url params
        url     = "invoiceitems" </> getInvoiceItemId invoiceitemid
        params  = toExpandable expandParams

------------------------------------------------------------------------------
-- | Update an `InvoiceItem` by `InvoiceItemId`
updateInvoiceItem
    :: InvoiceItemId     -- ^ `InvoiceItemId` of to update
    -> Maybe Amount      -- ^ `Amount` in cents of the charge to be applied to the invoice
    -> Maybe Description -- ^ `Amount` in cents of the charge to be applied to the invoice
    -> MetaData          -- ^ `MetaData` of `InvoiceItem` to update
    -> Stripe InvoiceItem
updateInvoiceItem
    invoiceitemid
    amount
    description
    metadata    = callAPI request
  where request = StripeRequest POST url params
        url     = "invoiceitems" </> getInvoiceItemId invoiceitemid
        params  = toMetaData metadata ++ getParams [
                         ("amount",  toText `fmap` amount)
                       , ("description",  description)
                       ]

------------------------------------------------------------------------------
-- | Delete an `InvoiceItem` by `InvoiceItemId`
deleteInvoiceItem
    :: InvoiceItemId -- ^ `InvoiceItemdId` of `InvoiceItem` to be deleted
    -> Stripe StripeDeleteResult
deleteInvoiceItem
    invoiceitemid = callAPI request
  where request = StripeRequest DELETE url params
        url     = "invoiceitems" </> getInvoiceItemId invoiceitemid
        params  = []

