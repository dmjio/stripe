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
-- {-\# LANGUAGE OverloadedStrings \#-}
-- import Web.Stripe
-- import Web.Stripe.Customer
--
-- main :: IO ()
-- main = do
--   let config = StripeConfig (StripeKey "secret_key")
--   result <- stripe config createCustomer
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
createCustomer :: StripeRequest CreateCustomer
createCustomer = request
  where request = mkStripeRequest POST url params
        url     = "customers"
        params  = []

data CreateCustomer
type instance StripeReturn CreateCustomer = Customer
instance StripeHasParam CreateCustomer AccountBalance
instance StripeHasParam CreateCustomer NewCard
instance StripeHasParam CreateCustomer TokenId
instance StripeHasParam CreateCustomer CouponId
instance StripeHasParam CreateCustomer Description
instance StripeHasParam CreateCustomer Email
instance StripeHasParam CreateCustomer MetaData
instance StripeHasParam CreateCustomer PlanId
instance StripeHasParam CreateCustomer Quantity
instance StripeHasParam CreateCustomer TrialEnd

------------------------------------------------------------------------------
-- | Retrieve a customer
getCustomer
    :: CustomerId  -- ^ `CustomerId` of `Customer` to retrieve
    -> StripeRequest GetCustomer
getCustomer
  customerid = request
  where request = mkStripeRequest GET url params
        url     = "customers" </> getCustomerId customerid
        params  = []

data GetCustomer
type instance StripeReturn GetCustomer = Customer
instance StripeHasParam GetCustomer ExpandParams

------------------------------------------------------------------------------
-- | Update a `Customer`
updateCustomer
    :: CustomerId -- ^ `CustomerId` of `Customer` to update
    -> StripeRequest UpdateCustomer
updateCustomer customerid = request
  where request = mkStripeRequest POST url params
        url     = "customers" </> getCustomerId customerid
        params  = []

data UpdateCustomer
type instance StripeReturn UpdateCustomer = Customer
instance StripeHasParam UpdateCustomer AccountBalance
instance StripeHasParam UpdateCustomer TokenId
instance StripeHasParam UpdateCustomer NewCard
instance StripeHasParam UpdateCustomer CouponId
instance StripeHasParam UpdateCustomer DefaultCard
instance StripeHasParam UpdateCustomer Description
instance StripeHasParam UpdateCustomer Email
instance StripeHasParam UpdateCustomer MetaData

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
getCustomers
    :: StripeRequest GetCustomers
getCustomers =
  request
  where request = mkStripeRequest GET url params
        url     = "customers"
        params  = []

data GetCustomers
type instance StripeReturn GetCustomers = (StripeList Customer)
instance StripeHasParam GetCustomers ExpandParams
instance StripeHasParam GetCustomers Created
instance StripeHasParam GetCustomers (EndingBefore CustomerId)
instance StripeHasParam GetCustomers Limit
instance StripeHasParam GetCustomers (StartingAfter CustomerId)

