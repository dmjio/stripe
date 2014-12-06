{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
-------------------------------------------
-- |
-- Module      : Web.Stripe.Customer
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- < https:/\/\stripe.com/docs/api#customers >
--
-- @
-- import Web.Stripe
-- import Web.Stripe.Customer
--
-- main :: IO ()
-- main = do
--   let config = SecretKey "secret_key"
--   result <- stripe config createEmptyCustomer
--   case result of
--     Right customer    -> print customer
--     Left  stripeError -> print stripeError
-- @
module Web.Stripe.Customer
    ( -- * API
      ---- * Create customer
      CreateCustomer
    , createCustomer
      ---- * Retrieve customer
    , GetCustomer
    , getCustomer
      ---- * Update customer
    , UpdateCustomer
    , updateCustomer
      ---- * Delete customer
    , DeleteCustomer
    , deleteCustomer
      ---- * List customers
    , GetCustomers
    , getCustomers
      -- * Types
    , AccountBalance     (..)
    , CardId             (..)
    , CardNumber         (..)
    , CouponId           (..)
    , Created            (..)
    , Customer           (..)
    , CustomerId         (..)
    , CVC                (..)
    , Description        (..)
    , Email              (..)
    , EndingBefore       (..)
    , ExpandParams       (..)
    , ExpMonth           (..)
    , ExpYear            (..)
    , Limit              (..)
    , MetaData           (..)
    , mkNewCard
    , NewCard            (..)
    , PlanId             (..)
    , Quantity           (..)
    , StartingAfter      (..)
    , StripeDeleteResult (..)
    , StripeList         (..)
    , TokenId            (..)
    , TrialEnd           (..)
    ) where

import           Web.Stripe.StripeRequest   (Method (GET, POST, DELETE),
                                             StripeHasParam, StripeRequest (..),
                                             StripeReturn, mkStripeRequest)
import           Web.Stripe.Util            ((</>))
import           Web.Stripe.Types           (AccountBalance(..), CVC (..),
                                             CardId (..), CardNumber (..),
                                             CouponId (..), Created(..), Customer (..),
                                             CustomerId (..), DefaultCard(..),
                                             Description(..), Email (..),
                                             EndingBefore(..), ExpMonth (..),
                                             ExpYear (..), Limit(..), PlanId (..),
                                             Quantity (..), MetaData(..),
                                             mkNewCard, NewCard(..), StartingAfter(..),
                                             StripeDeleteResult (..),
                                             StripeList (..), TokenId (..),
                                             TrialEnd(..), ExpandParams(..))
import           Web.Stripe.Types.Util

------------------------------------------------------------------------------
-- | Create a customer
data CreateCustomer
type instance StripeReturn CreateCustomer = Customer
instance StripeHasParam CreateCustomer AccountBalance
-- instance StripeHasParam CreateCustomer CardId -- FIXME: is a CardId actually valid or only TokenId?
instance StripeHasParam CreateCustomer NewCard
instance StripeHasParam CreateCustomer TokenId
instance StripeHasParam CreateCustomer CouponId
instance StripeHasParam CreateCustomer Description
instance StripeHasParam CreateCustomer Email
instance StripeHasParam CreateCustomer MetaData
instance StripeHasParam CreateCustomer PlanId
instance StripeHasParam CreateCustomer Quantity
instance StripeHasParam CreateCustomer TrialEnd

createCustomer :: StripeRequest CreateCustomer
createCustomer = request
  where request = mkStripeRequest POST url params
        url     = "customers"
        params  = []

------------------------------------------------------------------------------
-- | Retrieve a customer
data GetCustomer
type instance StripeReturn GetCustomer = Customer
instance StripeHasParam GetCustomer ExpandParams

getCustomer :: CustomerId -> StripeRequest GetCustomer
getCustomer
  customerid = request
  where request = mkStripeRequest GET url params
        url     = "customers" </> getCustomerId customerid
        params  = []

------------------------------------------------------------------------------
-- | Retrieve a `Customer`
data UpdateCustomer
type instance StripeReturn UpdateCustomer = Customer
instance StripeHasParam UpdateCustomer AccountBalance
-- instance StripeHasParam UpdateCustomer CardId
instance StripeHasParam UpdateCustomer TokenId
instance StripeHasParam UpdateCustomer NewCard
instance StripeHasParam UpdateCustomer CouponId
instance StripeHasParam UpdateCustomer DefaultCard
instance StripeHasParam UpdateCustomer Description
instance StripeHasParam UpdateCustomer Email
instance StripeHasParam UpdateCustomer MetaData

-- | Update a `Customer`
updateCustomer
    :: CustomerId
    -> StripeRequest UpdateCustomer
updateCustomer customerid = request
  where request = mkStripeRequest POST url params
        url     = "customers" </> getCustomerId customerid
        params  = []

------------------------------------------------------------------------------
-- | Deletes the specified `Customer`
data DeleteCustomer
type instance StripeReturn DeleteCustomer = StripeDeleteResult
deleteCustomer
    :: CustomerId -- ^ The `CustomerId` of the `Customer` to delete
    -> StripeRequest DeleteCustomer
deleteCustomer customerid = request
  where request = mkStripeRequest DELETE url params
        url     = "customers" </> getCustomerId customerid
        params  = []


------------------------------------------------------------------------------
-- | Retrieve up to 100 customers at a time
data GetCustomers
type instance StripeReturn GetCustomers = (StripeList Customer)
instance StripeHasParam GetCustomers ExpandParams
instance StripeHasParam GetCustomers Created
instance StripeHasParam GetCustomers (EndingBefore CustomerId)
instance StripeHasParam GetCustomers Limit
instance StripeHasParam GetCustomers (StartingAfter CustomerId)

-- | Retrieve up to 100 customers at a time
getCustomers
    :: StripeRequest GetCustomers
getCustomers =
  request
  where request = mkStripeRequest GET url params
        url     = "customers"
        params  = []
