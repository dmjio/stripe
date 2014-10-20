{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Web.Stripe.Subscription
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Web.Stripe.Subscription
    ( -- * Types
      CustomerId         (..)
    , CouponId           (..)
    , PlanId             (..)
    , Subscription       (..)
    , SubscriptionId     (..)
    , SubscriptionStatus (..)
    , StripeList         (..)
      -- * API
    , createSubscription
    , getSubscription
    , getSubscriptionExpandable
    , getSubscriptions
    , getSubscriptionsExpandable
    , updateSubscription
    , cancelSubscription
    ) where

import           Web.Stripe.Client.Internal (Method (GET, POST, DELETE), Stripe,
                                             StripeRequest (..), callAPI,
                                             getParams, toExpandable,
                                             toMetaData, toText, (</>))
import           Web.Stripe.Types           (CustomerId (..), EndingBefore,
                                             ExpandParams, Limit, MetaData,
                                             PlanId (..), StartingAfter, CouponId(..),
                                             Subscription (..), StripeList(..),
                                             SubscriptionId (..),
                                             SubscriptionStatus (..))
import           Web.Stripe.Types.Util

------------------------------------------------------------------------------
-- | Create a `Subscription` by `CustomerId` and `PlanId`
createSubscription
    :: CustomerId
    -> PlanId
    -> MetaData
    -> Stripe Subscription
createSubscription
    customerid
    (PlanId planid)
    metadata    = callAPI request
  where request = StripeRequest POST url params
        url     = "customers" </> getCustomerId customerid </> "subscriptions"
        params  = toMetaData metadata ++ getParams [ ("plan", Just planid)  ]

------------------------------------------------------------------------------
-- | Retrieve a `Subscription` by `CustomerId` and `SubscriptionId`
getSubscription
    :: CustomerId
    -> SubscriptionId
    -> Stripe Subscription
getSubscription
    customerid
    subscriptionid =
      getSubscriptionExpandable
        customerid subscriptionid []

------------------------------------------------------------------------------
-- | Retrieve a `Subscription` by `CustomerId` and `SubscriptionId` with `ExpandParams`
getSubscriptionExpandable
    :: CustomerId
    -> SubscriptionId
    -> ExpandParams
    -> Stripe Subscription
getSubscriptionExpandable
    customerid
    (SubscriptionId subscriptionid)
    expandParams = callAPI request
  where request = StripeRequest GET url params
        url     = "customers" </> getCustomerId customerid </> "subscriptions" </> subscriptionid
        params  = toExpandable expandParams

------------------------------------------------------------------------------
-- | Retrieve active `Subscription`s
getSubscriptions
    :: CustomerId
    -> Limit                        -- ^ Defaults to 10 if `Nothing` specified
    -> StartingAfter SubscriptionId -- ^ Paginate starting after the following `CustomerId`
    -> EndingBefore SubscriptionId  -- ^ Paginate ending before the following `CustomerId`
    -> Stripe (StripeList Subscription)
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
    :: CustomerId
    -> Limit                        -- ^ Defaults to 10 if `Nothing` specified
    -> StartingAfter SubscriptionId -- ^ Paginate starting after the following `CustomerId`
    -> EndingBefore SubscriptionId  -- ^ Paginate ending before the following `CustomerId`
    -> ExpandParams
    -> Stripe (StripeList Subscription)
getSubscriptionsExpandable
    customerid
    limit
    startingAfter
    endingBefore
    expandParams = callAPI request
  where request = StripeRequest GET url params
        url     = "customers" </> getCustomerId customerid </> "subscriptions"
        params  = getParams [
            ("limit", toText `fmap` limit )
          , ("starting_after", (\(SubscriptionId x) -> x) `fmap` startingAfter)
          , ("ending_before", (\(SubscriptionId x) -> x) `fmap` endingBefore)
          ] ++ toExpandable expandParams

------------------------------------------------------------------------------
-- | Update a `Subscription` by `CustomerId` and `SubscriptionId`
updateSubscription
    :: CustomerId
    -> SubscriptionId
    -> Maybe CouponId
    -> MetaData
    -> Stripe Subscription
updateSubscription
    customerid
    (SubscriptionId subscriptionid)
    couponid
    metadata    = callAPI request
  where request = StripeRequest POST url params
        url     = "customers" </> getCustomerId customerid </> "subscriptions" </> subscriptionid
        params  = toMetaData metadata ++ getParams [
           ("coupon", (\(CouponId x) -> x) `fmap` couponid)
          ]

------------------------------------------------------------------------------
-- | Delete a `Subscription` by `CustomerId` and `SubscriptionId`
cancelSubscription
    :: CustomerId
    -> SubscriptionId
    -> Bool -- ^ Flag set to true will delay cancellation until end of current period, default `False`
    -> Stripe Subscription
cancelSubscription
    customerid
    (SubscriptionId subscriptionid)
    atPeriodEnd
     = callAPI request
  where request = StripeRequest DELETE url params
        url     = "customers" </> getCustomerId customerid </> "subscriptions" </> subscriptionid
        params  = getParams [ ("at_period_end", Just $ toText atPeriodEnd) ]

