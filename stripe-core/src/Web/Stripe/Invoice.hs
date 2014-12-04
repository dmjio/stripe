{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
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
      CreateInvoice
    , createInvoice
    , GetInvoice
    , getInvoice
    , getInvoiceExpandable
    , GetInvoiceLineItems
    , getInvoiceLineItems
    , GetUpcomingInvoice
    , getUpcomingInvoice
    , UpdateInvoice
    , updateInvoice
    , PayInvoice
    , payInvoice
    , GetInvoices
    , getInvoices
    , getInvoicesExpandable
       -- * Types
    , ApplicationFeeId    (..)
    , Closed              (..)
    , CustomerId          (..)
    , Description         (..)
    , Discount            (..)
    , EndingBefore        (..)
    , Forgiven            (..)
    , Invoice             (..)
    , InvoiceId           (..)
    , InvoiceLineItem     (..)
    , InvoiceLineItemId   (..)
    , InvoiceLineItemType (..)
    , Limit               (..)
    , MetaData            (..)
    , Period              (..)
    , StatementDescription(..)
    , StartingAfter       (..)
    , StripeList          (..)
    , SubscriptionId      (..)
    ) where

import           Web.Stripe.StripeRequest (Method(GET, POST), StripeRequest(..),
                                           StripeReturn, StripeHasParam,
                                           toStripeParam, mkStripeRequest)
import           Web.Stripe.Util          ((</>), toExpandable)
import           Web.Stripe.Types         (ApplicationFeeId(..), Closed(..),
                                           CustomerId(..), Description(..),
                                           Discount(..), EndingBefore(..),
                                           ExpandParams, Forgiven(..),
                                           Invoice(..),InvoiceId(..),
                                           InvoiceLineItem(..),
                                           InvoiceLineItemId(..),
                                           InvoiceLineItemType(..),
                                           Limit(..), MetaData(..), Period(..),
                                           SubscriptionId(..), StartingAfter(..),
                                           StatementDescription(..),
                                           StripeList(..))
import           Web.Stripe.Types.Util    (getInvoiceId)

------------------------------------------------------------------------------
-- | The `Invoice` to be created for a `Customer`
data CreateInvoice
type instance StripeReturn CreateInvoice = Invoice
instance StripeHasParam CreateInvoice ApplicationFeeId
instance StripeHasParam CreateInvoice Description
instance StripeHasParam CreateInvoice MetaData
instance StripeHasParam CreateInvoice StatementDescription
instance StripeHasParam CreateInvoice SubscriptionId
createInvoice
    :: CustomerId -- ^ `CustomerId` of `Customer` to `Invoice`
    -> StripeRequest CreateInvoice
createInvoice
    customerid
                = request
  where request = mkStripeRequest POST url params
        url     = "invoices"
        params  = toStripeParam customerid $
                  []

------------------------------------------------------------------------------
-- | Retrieve an `Invoice` by `InvoiceId`
data GetInvoice
type instance StripeReturn GetInvoice = Invoice
getInvoice
    :: InvoiceId -- ^ Get an `Invoice` by `InvoiceId`
    -> StripeRequest GetInvoice
getInvoice
    invoiceid = getInvoiceExpandable invoiceid []

------------------------------------------------------------------------------
-- | Retrieve an `Invoice` by `InvoiceId` with `ExpandParams`
getInvoiceExpandable
    :: InvoiceId    -- ^ Get an `Invoice` by `InvoiceId`
    -> ExpandParams -- ^ `ExpandParams` of the objects for expansion
    -> StripeRequest GetInvoice
getInvoiceExpandable
    invoiceid
    expandParams = request
  where request = mkStripeRequest GET url params
        url     = "invoices" </> getInvoiceId invoiceid
        params  = toExpandable expandParams

------------------------------------------------------------------------------
-- | Retrieve a `StripeList` of `Invoice`s
data GetInvoices
type instance StripeReturn GetInvoices = StripeList Invoice
instance StripeHasParam GetInvoices (EndingBefore InvoiceId)
instance StripeHasParam GetInvoices Limit
instance StripeHasParam GetInvoices (StartingAfter InvoiceId)
getInvoices
    :: StripeRequest GetInvoices
getInvoices =
      getInvoicesExpandable []

------------------------------------------------------------------------------
-- | Retrieve a `StripeList` of `Invoice`s with `ExpandParams`
getInvoicesExpandable
    :: ExpandParams                -- ^ `ExpandParams` of the objects for expansion
    -> StripeRequest GetInvoices
getInvoicesExpandable
    expandParams = request
  where request = mkStripeRequest GET url params
        url     = "invoices"
        params  = toExpandable expandParams

------------------------------------------------------------------------------
-- | Retrieve an `Invoice` by `InvoiceId`
data GetInvoiceLineItems
type instance StripeReturn GetInvoiceLineItems = StripeList InvoiceLineItem
instance StripeHasParam GetInvoiceLineItems CustomerId
instance StripeHasParam GetInvoiceLineItems (EndingBefore InvoiceLineItemId)
instance StripeHasParam GetInvoiceLineItems Limit
instance StripeHasParam GetInvoiceLineItems (StartingAfter InvoiceLineItemId)
instance StripeHasParam GetInvoiceLineItems SubscriptionId
getInvoiceLineItems
    :: InvoiceId                       -- ^ Get an `Invoice` by `InvoiceId`
    -> StripeRequest GetInvoiceLineItems
getInvoiceLineItems
    invoiceid   = request
  where request = mkStripeRequest GET url params
        url     = "invoices" </> getInvoiceId invoiceid </> "lines"
        params  = []

------------------------------------------------------------------------------
-- | Retrieve an upcoming `Invoice` for a `Customer` by `CustomerId`
data GetUpcomingInvoice
type instance StripeReturn GetUpcomingInvoice = Invoice
instance StripeHasParam GetUpcomingInvoice SubscriptionId
getUpcomingInvoice
    :: CustomerId -- ^ The `InvoiceId` of the `Invoice` to retrieve
    -> StripeRequest GetUpcomingInvoice
getUpcomingInvoice
    customerid = request
  where request = mkStripeRequest GET url params
        url     = "invoices" </> "upcoming"
        params  = toStripeParam customerid []

------------------------------------------------------------------------------
-- | Update `Invoice` by `InvoiceId`
data UpdateInvoice
type instance StripeReturn UpdateInvoice = Invoice
instance StripeHasParam UpdateInvoice ApplicationFeeId
instance StripeHasParam UpdateInvoice Closed
instance StripeHasParam UpdateInvoice Description
instance StripeHasParam UpdateInvoice Forgiven
instance StripeHasParam UpdateInvoice MetaData
instance StripeHasParam UpdateInvoice StatementDescription
updateInvoice
    :: InvoiceId -- ^ The `InvoiceId` of the `Invoice` to update
    -> StripeRequest UpdateInvoice
updateInvoice
    invoiceid   = request
  where request = mkStripeRequest POST url params
        url     = "invoices" </> getInvoiceId invoiceid
        params  = []

------------------------------------------------------------------------------
-- | Pay `Invoice` by `InvoiceId`
data PayInvoice
type instance StripeReturn PayInvoice = Invoice
payInvoice
    :: InvoiceId -- ^ The `InvoiceId` of the `Invoice` to pay
    -> StripeRequest PayInvoice
payInvoice
    invoiceid   = request
  where request = mkStripeRequest POST url params
        url     = "invoices" </> getInvoiceId invoiceid </> "pay"
        params  = []
