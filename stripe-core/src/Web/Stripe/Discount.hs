{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
-------------------------------------------
-- |
-- Module      : Web.Stripe.Discount
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- < https:/\/\stripe.com/docs/api#discounts >
--
-- @
-- import Web.Stripe
-- import Web.Stripe.Discount
-- import Web.Stripe.Customer
--
-- main :: IO ()
-- main = do
--   let config = SecretKey "secret_key"
--   result <- stripe config $ deleteCustomerDiscount (CustomerId "customer_id")
--   case result of
--     Right deleteResult -> print deleteResult
--     Left  stripeError  -> print stripeError
-- @
module Web.Stripe.Discount
    ( -- * API
      DeleteCustomerDiscount
    , deleteCustomerDiscount
    , DeleteSubscriptionDiscount
    , deleteSubscriptionDiscount
      -- * Types
    , StripeDeleteResult (..)
    , CustomerId         (..)
    , SubscriptionId     (..)
    , Discount           (..)
    ) where

import           Web.Stripe.StripeRequest   (Method (DELETE), StripeReturn,
                                            StripeRequest (..), mkStripeRequest)
import           Web.Stripe.Util    ((</>))
import           Web.Stripe.Types           (CustomerId (..), Discount(..),
                                             StripeDeleteResult (..),
                                             SubscriptionId (..))
import           Web.Stripe.Types.Util      (getCustomerId)

------------------------------------------------------------------------------
-- | Delete `Customer` `Discount` by `CustomerId`
data DeleteCustomerDiscount
type instance StripeReturn DeleteCustomerDiscount = StripeDeleteResult
deleteCustomerDiscount
    :: CustomerId -- ^ The `Customer` upon which to remove the `Discount`
    -> StripeRequest DeleteCustomerDiscount
deleteCustomerDiscount
    customerId = request
  where request = mkStripeRequest DELETE url params
        url     = "customers" </> getCustomerId customerId </> "discount"
        params  = []

------------------------------------------------------------------------------
-- | Delete `Subscription` `Discount` by `CustomerId` and `SubscriptionId`
data DeleteSubscriptionDiscount
type instance StripeReturn DeleteSubscriptionDiscount = StripeDeleteResult
deleteSubscriptionDiscount
  :: CustomerId     -- ^ The `Customer` to remove the `Discount` from
  -> SubscriptionId -- ^ The `Subscription` to remove the `Discount` from
  -> StripeRequest DeleteSubscriptionDiscount
deleteSubscriptionDiscount
    customerId
    (SubscriptionId subId) = request
  where request = mkStripeRequest DELETE url params
        url     = "customers" </> getCustomerId customerId </> "subscriptions" </> subId </> "discount"
        params  = []
