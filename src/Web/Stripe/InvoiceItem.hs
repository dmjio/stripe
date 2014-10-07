{-# LANGUAGE OverloadedStrings #-}
module Web.Stripe.InvoiceItem
    ( -- * API 
      createInvoiceItem
    , getInvoiceItem
    , getInvoiceItems
    , updateInvoiceItem
    , deleteInvoiceItem
      -- * Types
    , InvoiceItemId      (..)
    , InvoiceItem        (..)
    , CustomerId         (..)
    , Currency           (..)
    , InvoiceId          (..)
    , SubscriptionId     (..)
    , Description        (..)
    , StripeDeleteResult (..)
    , Amount
    ) where

import           Web.Stripe.Client.Internal (Method (GET, POST, DELETE), Stripe,
                                             StripeRequest (..), callAPI, toMetaData,
                                             getParams, toText, (</>))
import           Web.Stripe.Types           (Amount, Currency (..),
                                             CustomerId (..), Description (..),
                                             InvoiceId (..), InvoiceItem (..),
                                             InvoiceItemId (..), Limit, StartingAfter, EndingBefore,
                                             StripeDeleteResult (..),
                                             SubscriptionId (..), MetaData)

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
    (CustomerId customerId)
    amount
    (Currency currency)
    invoiceId
    subscriptionId
    description
    metadata    = callAPI request
  where request = StripeRequest POST url params
        url     = "invoiceitems"
        params  = toMetaData metadata ++ getParams [
                    ("customer", Just customerId)
                  , ("amount", toText `fmap` Just amount)
                  , ("currency", Just currency)
                  , ("invoice", (\(InvoiceId x) -> x) `fmap` invoiceId)
                  , ("subscription", (\(SubscriptionId x) -> x) `fmap` subscriptionId)
                  , ("description", description)
                  ]

------------------------------------------------------------------------------
-- | Retrieve an `InvoiceItem` by `InvoiceItemId`
getInvoiceItems
    :: InvoiceItemId               -- ^ `InvoiceItemId` of `InvoiceItem` to retrieve
    -> Limit                       -- ^ Defaults to 10 if `Nothing` specified
    -> StartingAfter InvoiceItemId -- ^ Paginate starting after the following `InvoiceItemId`
    -> EndingBefore InvoiceItemId  -- ^ Paginate ending before the following `InvoiceItemId`
    -> Stripe InvoiceItem
getInvoiceItems
    (InvoiceItemId itemId)
    limit
    startingAfter
    endingBefore = callAPI request
  where request = StripeRequest GET url params
        url     = "invoiceitems" </> itemId
        params  = getParams [
            ("limit", toText `fmap` limit )
          , ("starting_after", (\(InvoiceItemId x) -> x) `fmap` startingAfter)
          , ("ending_before", (\(InvoiceItemId x) -> x) `fmap` endingBefore)
          ]

------------------------------------------------------------------------------
-- | Retrieve an `InvoiceItem` by `InvoiceItemId`
getInvoiceItem
    :: InvoiceItemId -- ^ `InvoiceItemId` of `InvoiceItem` to retrieve
    -> Stripe InvoiceItem
getInvoiceItem
    (InvoiceItemId itemId) = callAPI request
  where request = StripeRequest GET url params
        url     = "invoiceitems" </> itemId
        params  = []

------------------------------------------------------------------------------
-- | Update an `InvoiceItem` by `InvoiceItemId`
updateInvoiceItem
    :: InvoiceItemId  -- ^ `InvoiceItemId` of `InvoiceItem` to update
    -> MetaData       -- ^ `MetaData` of `InvoiceItem` to update
    -> Stripe InvoiceItem
updateInvoiceItem
    (InvoiceItemId itemId)
    metadata    = callAPI request
  where request = StripeRequest POST url params
        url     = "invoiceitems" </> itemId
        params  = toMetaData metadata

------------------------------------------------------------------------------
-- | Delete an `InvoiceItem` by `InvoiceItemId`
deleteInvoiceItem
    :: InvoiceItemId -- ^ `InvoiceItemdId` of `InvoiceItem` to be deleted
    -> Stripe StripeDeleteResult
deleteInvoiceItem
    (InvoiceItemId itemId) = callAPI request
  where request = StripeRequest DELETE url params
        url     = "invoiceitems" </> itemId
        params  = []

