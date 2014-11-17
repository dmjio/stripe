{-# LANGUAGE OverloadedStrings #-}
-------------------------------------------
-- |
-- Module      : Web.Stripe.Subscription
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- < https:/\/\stripe.com/docs/api#subscriptions >
--
-- @
-- import Web.Stripe
-- import Web.Stripe.Subscription
-- import Web.Stripe.Customer
-- import Web.Stripe.Plan
--
-- main :: IO ()
-- main = do
--   let config = SecretKey "secret_key"
--   result <- stripe config $ do
--     Customer { customerId = cid } <- createEmptyCustomer
--     Plan { planId = pid } <- createPlan (PlanId "free plan")
--                      (0 :: Amount) -- free plan
--                      (USD :: Currency)
--                      (Month :: Inteval)
--                      ("sample plan" :: Name)
--                      ([] :: MetaData)
--   createSubscription cid pid ([] :: MetaData)
--   case result of
--     Right subscription -> print subscription
--     Left stripeError -> print stripeError
-- @
module Web.Stripe.Subscription
    ( -- * API
      createSubscription
    , getSubscription
    , getSubscriptionExpandable
    , getSubscriptions
    , getSubscriptionsExpandable
    , updateSubscription
    , cancelSubscription
      -- * Types
    , Subscription       (..)
    , SubscriptionId     (..)
    , SubscriptionStatus (..)
    , CustomerId         (..)
    , CouponId           (..)
    , Coupon             (..)
    , PlanId             (..)
    , StripeList         (..)
    ) where

import           Web.Stripe.StripeRequest    (Method (GET, POST, DELETE),
                                             StripeRequest (..), mkStripeRequest)
import           Web.Stripe.Util     (getParams, toExpandable,
                                             toMetaData, toText, (</>))
import           Web.Stripe.Types           (CustomerId (..), EndingBefore,
                                             ExpandParams, Limit, MetaData,
                                             PlanId (..), StartingAfter, CouponId(..),
                                             Subscription (..), StripeList(..),
                                             SubscriptionId (..), Coupon(..),
                                             SubscriptionStatus (..))
import           Web.Stripe.Types.Util      (getCustomerId)

------------------------------------------------------------------------------
-- | Create a `Subscription` by `CustomerId` and `PlanId`
createSubscription
    :: CustomerId -- ^ The `CustomerId` upon which to create the `Subscription`
    -> PlanId     -- ^ The `PlanId` to associate the `Subscription` with
    -> MetaData   -- ^ The `MetaData` associated with the `Subscription`
    -> StripeRequest Subscription
createSubscription
    customerid
    (PlanId planid)
    metadata    = request
  where request = mkStripeRequest POST url params
        url     = "customers" </> getCustomerId customerid </> "subscriptions"
        params  = toMetaData metadata ++ getParams [ ("plan", Just planid)  ]

------------------------------------------------------------------------------
-- | Retrieve a `Subscription` by `CustomerId` and `SubscriptionId`
getSubscription
    :: CustomerId       -- ^ The `CustomerId` of the `Subscription`
    -> SubscriptionId   -- ^ The `SubscriptionId` of the `Subscription` to retrieve
    -> StripeRequest Subscription
getSubscription
    customerid
    subscriptionid =
      getSubscriptionExpandable
        customerid subscriptionid []

------------------------------------------------------------------------------
-- | Retrieve a `Subscription` by `CustomerId` and `SubscriptionId` with `ExpandParams`
getSubscriptionExpandable
    :: CustomerId      -- ^ The `CustomerId` of the `Subscription` to retrieve
    -> SubscriptionId  -- ^ The `SubscriptionId` of the `Subscription` to retrieve
    -> ExpandParams    -- ^ The `ExpandParams` of the object to expand
    -> StripeRequest Subscription
getSubscriptionExpandable
    customerid
    (SubscriptionId subscriptionid)
    expandParams = request
  where request = mkStripeRequest GET url params
        url     = "customers" </> getCustomerId customerid </> "subscriptions" </> subscriptionid
        params  = toExpandable expandParams

------------------------------------------------------------------------------
-- | Retrieve active `Subscription`s
getSubscriptions
    :: CustomerId                   -- ^ The `CustomerId` of the `Subscription`s to retrieve
    -> Limit                        -- ^ Defaults to 10 if `Nothing` specified
    -> StartingAfter SubscriptionId -- ^ Paginate starting after the following `CustomerId`
    -> EndingBefore SubscriptionId  -- ^ Paginate ending before the following `CustomerId`
    -> StripeRequest (StripeList Subscription)
getSubscriptions
    customerid
    limit
    startingAfter
    endingBefore =
      getSubscriptionsExpandable customerid limit
        startingAfter endingBefore []

------------------------------------------------------------------------------
-- | Retrieve active `Subscription`s
getSubscriptionsExpandable
    :: CustomerId                   -- ^ The `CustomerId` of the `Subscription`s to retrieve
    -> Limit                        -- ^ Defaults to 10 if `Nothing` specified
    -> StartingAfter SubscriptionId -- ^ Paginate starting after the following `CustomerId`
    -> EndingBefore SubscriptionId  -- ^ Paginate ending before the following `CustomerId`
    -> ExpandParams                 -- ^ The `ExpandParams` of the object to expand
    -> StripeRequest (StripeList Subscription)
getSubscriptionsExpandable
    customerid
    limit
    startingAfter
    endingBefore
    expandParams = request
  where request = mkStripeRequest GET url params
        url     = "customers" </> getCustomerId customerid </> "subscriptions"
        params  = getParams [
            ("limit", toText `fmap` limit )
          , ("starting_after", (\(SubscriptionId x) -> x) `fmap` startingAfter)
          , ("ending_before", (\(SubscriptionId x) -> x) `fmap` endingBefore)
          ] ++ toExpandable expandParams

------------------------------------------------------------------------------
-- | Update a `Subscription` by `CustomerId` and `SubscriptionId`
updateSubscription
    :: CustomerId      -- ^ The `CustomerId` of the `Subscription` to update
    -> SubscriptionId  -- ^ The `SubscriptionId` of the `Subscription` to update
    -> Maybe CouponId  -- ^ Optional: The `Coupon` of the `Subscription` to update
    -> MetaData
    -> StripeRequest Subscription
updateSubscription
    customerid
    (SubscriptionId subscriptionid)
    couponid
    metadata    = request
  where request = mkStripeRequest POST url params
        url     = "customers" </> getCustomerId customerid </> "subscriptions" </> subscriptionid
        params  = toMetaData metadata ++ getParams [
           ("coupon", (\(CouponId x) -> x) `fmap` couponid)
          ]

------------------------------------------------------------------------------
-- | Delete a `Subscription` by `CustomerId` and `SubscriptionId`
cancelSubscription
    :: CustomerId     -- ^ The `CustomerId` of the `Subscription` to cancel
    -> SubscriptionId -- ^ The `SubscriptionId` of the `Subscription` to cancel
    -> Bool           -- ^ Flag set to true will delay cancellation until end of current period, default `False`
    -> StripeRequest Subscription
cancelSubscription
    customerid
    (SubscriptionId subscriptionid)
    atPeriodEnd
     = request
  where request = mkStripeRequest DELETE url params
        url     = "customers" </> getCustomerId customerid </> "subscriptions" </> subscriptionid
        params  = getParams [ ("at_period_end", Just $ toText atPeriodEnd) ]
