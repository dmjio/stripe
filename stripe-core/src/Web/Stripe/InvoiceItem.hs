{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
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
-- {-\# LANGUAGE OverloadedStrings \#-}
-- import Web.Stripe
-- import Web.Stripe.Customer
-- import Web.Stripe.InvoiceItem
--
-- main :: IO ()
-- main = do
--   let config = StripeConfig (StripeKey "secret_key")
--   result <- stripe config $ createCustomer
--   case result of
--     (Left stripeError) -> print stripeError
--     (Right (Customer { customerId = cid })) ->
--       do result <- stripe config $
--            createInvoiceItem cid (Amount 100) USD
--              -&- (Description "description")
--          case result of
--            Left stripeError  -> print stripeError
--            Right invoiceitem -> print invoiceitem
-- @
module Web.Stripe.InvoiceItem
    ( -- * API
      CreateInvoiceItem
    , createInvoiceItem
    , GetInvoiceItem
    , getInvoiceItem
    , UpdateInvoiceItem
    , updateInvoiceItem
    , DeleteInvoiceItem
    , deleteInvoiceItem
    , GetInvoiceItems
    , getInvoiceItems
      -- * Types
    , InvoiceItemId      (..)
    , InvoiceItem        (..)
    , Created            (..)
    , CustomerId         (..)
    , Currency           (..)
    , EndingBefore       (..)
    , ExpandParams       (..)
    , InvoiceId          (..)
    , Invoice            (..)
    , Limit              (..)
    , SubscriptionId     (..)
    , StartingAfter      (..)
    , StripeDeleteResult (..)
    , StripeList         (..)
    , Description        (..)
    , Amount             (..)
    ) where
import           Web.Stripe.StripeRequest (Method (GET, POST, DELETE),
                                           StripeHasParam, StripeRequest (..),
                                           StripeReturn, ToStripeParam(..),
                                           mkStripeRequest)
import           Web.Stripe.Util          ((</>))
import           Web.Stripe.Types         (Amount(..), Created(..), Currency (..),
                                           CustomerId (..), Description(..),
                                           InvoiceId (..), InvoiceItem (..), Invoice(..),
                                           InvoiceItemId (..), Limit(..), StartingAfter(..), EndingBefore(..),
                                           StripeDeleteResult (..), ExpandParams(..),
                                           SubscriptionId (..), StripeList(..), MetaData(..))
import           Web.Stripe.Types.Util    (getInvoiceItemId)

------------------------------------------------------------------------------
-- | Create an invoice for a Customer
createInvoiceItem
    :: CustomerId            -- ^ `CustomerId` of `Customer` on which to create an `InvoiceItem`
    -> Amount                -- ^ `Amount` associated with `InvoiceItem`
    -> Currency              -- ^ `Currency` to use for `InvoiceItem`
    -> StripeRequest CreateInvoiceItem
createInvoiceItem
    customerid
    amount
    currency    = request
  where request = mkStripeRequest POST url params
        url     = "invoiceitems"
        params  = toStripeParam customerid $
                  toStripeParam amount     $
                  toStripeParam currency   $
                  []

data CreateInvoiceItem
type instance StripeReturn CreateInvoiceItem = InvoiceItem
instance StripeHasParam CreateInvoiceItem InvoiceId
instance StripeHasParam CreateInvoiceItem SubscriptionId
instance StripeHasParam CreateInvoiceItem Description
instance StripeHasParam CreateInvoiceItem MetaData

------------------------------------------------------------------------------
-- | Retrieve an `InvoiceItem` by `InvoiceItemId`
getInvoiceItem
    :: InvoiceItemId -- ^ `InvoiceItemId` of `InvoiceItem` to retrieve
    -> StripeRequest GetInvoiceItem
getInvoiceItem
  invoiceitemid = request
  where request = mkStripeRequest GET url params
        url     = "invoiceitems" </> getInvoiceItemId invoiceitemid
        params  = []

data GetInvoiceItem
type instance StripeReturn GetInvoiceItem = InvoiceItem
instance StripeHasParam GetInvoiceItem ExpandParams

------------------------------------------------------------------------------
-- | Update an `InvoiceItem` by `InvoiceItemId`
updateInvoiceItem
    :: InvoiceItemId     -- ^ `InvoiceItemId` of to update
    -> StripeRequest UpdateInvoiceItem
updateInvoiceItem
    invoiceitemid
                = request
  where request = mkStripeRequest POST url params
        url     = "invoiceitems" </> getInvoiceItemId invoiceitemid
        params  = []

data UpdateInvoiceItem
type instance StripeReturn UpdateInvoiceItem = InvoiceItem
instance StripeHasParam UpdateInvoiceItem Amount
instance StripeHasParam UpdateInvoiceItem Description
instance StripeHasParam UpdateInvoiceItem MetaData

------------------------------------------------------------------------------
-- | Delete an `InvoiceItem` by `InvoiceItemId`
deleteInvoiceItem
    :: InvoiceItemId -- ^ `InvoiceItemdId` of `InvoiceItem` to be deleted
    -> StripeRequest DeleteInvoiceItem
deleteInvoiceItem
    invoiceitemid = request
  where request = mkStripeRequest DELETE url params
        url     = "invoiceitems" </> getInvoiceItemId invoiceitemid
        params  = []

data DeleteInvoiceItem
type instance StripeReturn DeleteInvoiceItem = StripeDeleteResult

------------------------------------------------------------------------------
-- | List `InvoiceItem`s
getInvoiceItems
    :: StripeRequest GetInvoiceItems
getInvoiceItems = request
  where request = mkStripeRequest GET url params
        url     = "invoiceitems"
        params  = []

data GetInvoiceItems
type instance StripeReturn GetInvoiceItems = (StripeList InvoiceItem)
instance StripeHasParam GetInvoiceItems ExpandParams
instance StripeHasParam GetInvoiceItems Created
instance StripeHasParam GetInvoiceItems CustomerId
instance StripeHasParam GetInvoiceItems (EndingBefore InvoiceItemId)
instance StripeHasParam GetInvoiceItems Limit
instance StripeHasParam GetInvoiceItems (StartingAfter InvoiceItemId)
instance StripeHasParam GetInvoiceItems InvoiceId
