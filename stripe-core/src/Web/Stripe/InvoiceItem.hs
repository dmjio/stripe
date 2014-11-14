{-# LANGUAGE OverloadedStrings #-}
-------------------------------------------
-- |
-- Module      : Web.Stripe.InvoiceItem
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- < https:/\/\stripe.com/docs/api#invoiceitems >
--
-- @
-- import Web.Stripe
-- import Web.Stripe.Customer
-- import Web.Stripe.InvoiceItem
--
-- main :: IO ()
-- main = do
--   let config = SecretKey "secret_key"
--   result <- stripe config $ do
--     Customer { customerId = cid } <- createEmptyCustomer
--     createInvoiceItem cid 100 USD Nothing Nothing (Just "description") []
--   case result of
--     Right invoiceitem -> print invoiceitem
--     Left  stripeError -> print stripeError
-- @
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

import           Web.Stripe.Client.Types   (Method (GET, POST, DELETE),
                                            StripeRequest(..), mkStripeRequest)
import           Web.Stripe.Client.Util    ( toMetaData, toExpandable,
                                             getParams, toText, (</>), toTextLower)
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
    -> StripeRequest InvoiceItem
createInvoiceItem
    customerid
    amount
    currency
    invoiceid
    subscriptionId
    description
    metadata    = request
  where request = mkStripeRequest POST url params
        url     = "invoiceitems"
        params  = toMetaData metadata ++ getParams [
                    ("customer", Just $ getCustomerId customerid)
                  , ("amount", toText `fmap` Just amount)
                  , ("currency", toTextLower `fmap` Just currency)
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
    -> StripeRequest (StripeList InvoiceItem)
getInvoiceItems
    customerid
    limit
    startingAfter
    endingBefore = request
  where request = mkStripeRequest GET url params
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
    -> ExpandParams                -- ^ `ExpandParams` of the objects for expansion
    -> StripeRequest (StripeList InvoiceItem)
getInvoiceItemsExpandable
    customerid
    limit
    startingAfter
    endingBefore
    expandParams = request
  where request = mkStripeRequest GET url params
        url     = "invoiceitems"
        params  = getParams [
            ("limit", toText `fmap` limit )
          , ("starting_after", (\(InvoiceItemId x) -> x) `fmap` startingAfter)
          , ("ending_before", (\(InvoiceItemId x) -> x) `fmap` endingBefore)
          , ("customer", (\(CustomerId x) -> x) `fmap` customerid)
          ] ++ toExpandable expandParams

------------------------------------------------------------------------------
-- | Retrieve an `InvoiceItem` by `InvoiceItemId`
getInvoiceItem
    :: InvoiceItemId -- ^ `InvoiceItemId` of `InvoiceItem` to retrieve
    -> StripeRequest InvoiceItem
getInvoiceItem
    invoiceitemid =
      getInvoiceItemExpandable invoiceitemid []

------------------------------------------------------------------------------
-- | Retrieve an `InvoiceItem` by `InvoiceItemId`
getInvoiceItemExpandable
    :: InvoiceItemId -- ^ `InvoiceItemId` of `InvoiceItem` to retrieve
    -> ExpandParams  -- ^ `ExpandParams` of the objects for expansion
    -> StripeRequest InvoiceItem
getInvoiceItemExpandable
    invoiceitemid
    expandParams = request
  where request = mkStripeRequest GET url params
        url     = "invoiceitems" </> getInvoiceItemId invoiceitemid
        params  = toExpandable expandParams

------------------------------------------------------------------------------
-- | Update an `InvoiceItem` by `InvoiceItemId`
updateInvoiceItem
    :: InvoiceItemId     -- ^ `InvoiceItemId` of to update
    -> Maybe Amount      -- ^ `Amount` in cents of the charge to be applied to the invoice
    -> Maybe Description -- ^ `Amount` in cents of the charge to be applied to the invoice
    -> MetaData          -- ^ `MetaData` of `InvoiceItem` to update
    -> StripeRequest InvoiceItem
updateInvoiceItem
    invoiceitemid
    amount
    description
    metadata    = request
  where request = mkStripeRequest POST url params
        url     = "invoiceitems" </> getInvoiceItemId invoiceitemid
        params  = toMetaData metadata ++ getParams [
                         ("amount",  toText `fmap` amount)
                       , ("description",  description)
                       ]

------------------------------------------------------------------------------
-- | Delete an `InvoiceItem` by `InvoiceItemId`
deleteInvoiceItem
    :: InvoiceItemId -- ^ `InvoiceItemdId` of `InvoiceItem` to be deleted
    -> StripeRequest StripeDeleteResult
deleteInvoiceItem
    invoiceitemid = request
  where request = mkStripeRequest DELETE url params
        url     = "invoiceitems" </> getInvoiceItemId invoiceitemid
        params  = []
