{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
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
      CreateSubscription
    , createSubscription
    , GetSubscription
    , getSubscription
    , getSubscriptionExpandable
    , UpdateSubscription
    , updateSubscription
    , CancelSubscription
    , cancelSubscription
    , GetSubscriptions
    , getSubscriptions
    , getSubscriptionsExpandable
      -- * Types
    , ApplicationFeePercent (..)
    , AtPeriodEnd        (..)
    , CustomerId         (..)
    , CouponId           (..)
    , Coupon             (..)
    , EndingBefore       (..)
    , Limit              (..)
    , MetaData           (..)
    , PlanId             (..)
    , Prorate            (..)
    , Quantity           (..)
    , StartingAfter      (..)
    , StripeList         (..)
    , Subscription       (..)
    , SubscriptionId     (..)
    , SubscriptionStatus (..)
    , TrialEnd           (..)
    ) where

import           Web.Stripe.StripeRequest   (Method (GET, POST, DELETE),
                                             StripeHasParam, StripeReturn,
                                             StripeRequest (..),
                                             ToStripeParam(..), mkStripeRequest)
import           Web.Stripe.Util            (toExpandable, (</>))
import           Web.Stripe.Types           (ApplicationFeePercent(..),
                                             AtPeriodEnd(..), CardId(..),
                                             CustomerId (..), Coupon(..),
                                             CouponId(..), EndingBefore(..),
                                             ExpandParams, Limit(..), MetaData(..),
                                             PlanId (..), Prorate(..), Quantity(..),
                                             StartingAfter(..),
                                             Subscription (..), StripeList(..),
                                             SubscriptionId (..),
                                             SubscriptionStatus (..), TrialEnd(..))
import           Web.Stripe.Types.Util      (getCustomerId)

------------------------------------------------------------------------------
-- | Create a `Subscription` by `CustomerId` and `PlanId`
data CreateSubscription
type instance StripeReturn CreateSubscription = Subscription
instance StripeHasParam CreateSubscription CouponId
instance StripeHasParam CreateSubscription TrialEnd
instance StripeHasParam CreateSubscription CardId
instance StripeHasParam CreateSubscription Quantity
instance StripeHasParam CreateSubscription ApplicationFeePercent
instance StripeHasParam CreateSubscription MetaData

createSubscription
    :: CustomerId -- ^ The `CustomerId` upon which to create the `Subscription`
    -> PlanId     -- ^ The `PlanId` to associate the `Subscription` with
    -> StripeRequest CreateSubscription
createSubscription
    customerid
    planId = request
  where request = mkStripeRequest POST url params
        url     = "customers" </> getCustomerId customerid </> "subscriptions"
        params  = toStripeParam planId []

------------------------------------------------------------------------------
-- | Retrieve a `Subscription` by `CustomerId` and `SubscriptionId`
data GetSubscription
type instance StripeReturn GetSubscription = Subscription
getSubscription
    :: CustomerId       -- ^ The `CustomerId` of the `Subscription`
    -> SubscriptionId   -- ^ The `SubscriptionId` of the `Subscription` to retrieve
    -> StripeRequest GetSubscription
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
    -> StripeRequest GetSubscription
getSubscriptionExpandable
    customerid
    (SubscriptionId subscriptionid)
    expandParams = request
  where request = mkStripeRequest GET url params
        url     = "customers" </> getCustomerId customerid </> "subscriptions" </> subscriptionid
        params  = toExpandable expandParams

-- | Update a `Subscription` by `CustomerId` and `SubscriptionId`
data UpdateSubscription
type instance StripeReturn UpdateSubscription = Subscription
instance StripeHasParam UpdateSubscription PlanId
instance StripeHasParam UpdateSubscription CouponId
instance StripeHasParam UpdateSubscription Prorate
instance StripeHasParam UpdateSubscription TrialEnd
instance StripeHasParam UpdateSubscription CardId
instance StripeHasParam UpdateSubscription Quantity
instance StripeHasParam UpdateSubscription ApplicationFeePercent
instance StripeHasParam UpdateSubscription MetaData
updateSubscription
    :: CustomerId      -- ^ The `CustomerId` of the `Subscription` to update
    -> SubscriptionId  -- ^ The `SubscriptionId` of the `Subscription` to update
    -> StripeRequest UpdateSubscription
updateSubscription
   customerid
    (SubscriptionId subscriptionid)
  = request
  where request = mkStripeRequest POST url params
        url     = "customers" </> getCustomerId customerid </> "subscriptions" </> subscriptionid
        params  = []

------------------------------------------------------------------------------
-- | Delete a `Subscription` by `CustomerId` and `SubscriptionId`
data CancelSubscription
instance StripeHasParam CancelSubscription AtPeriodEnd
type instance StripeReturn CancelSubscription = Subscription
cancelSubscription
    :: CustomerId     -- ^ The `CustomerId` of the `Subscription` to cancel
    -> SubscriptionId -- ^ The `SubscriptionId` of the `Subscription` to cancel
    -> StripeRequest CancelSubscription
cancelSubscription
    customerid
    (SubscriptionId subscriptionid)
     = request
  where request = mkStripeRequest DELETE url params
        url     = "customers" </> getCustomerId customerid </> "subscriptions" </> subscriptionid
        params  = []

------------------------------------------------------------------------------
-- | Retrieve active `Subscription`s
data GetSubscriptions
type instance StripeReturn GetSubscriptions = StripeList Subscription
instance StripeHasParam GetSubscriptions (EndingBefore SubscriptionId)
instance StripeHasParam GetSubscriptions Limit
instance StripeHasParam GetSubscriptions (StartingAfter SubscriptionId)
getSubscriptions
    :: CustomerId                   -- ^ The `CustomerId` of the `Subscription`s to retrieve
    -> StripeRequest GetSubscriptions
getSubscriptions
    customerid =
      getSubscriptionsExpandable customerid []

------------------------------------------------------------------------------
-- | Retrieve active `Subscription`s
getSubscriptionsExpandable
    :: CustomerId                   -- ^ The `CustomerId` of the `Subscription`s to retrieve
    -> ExpandParams                 -- ^ The `ExpandParams` of the object to expand
    -> StripeRequest GetSubscriptions
getSubscriptionsExpandable
    customerid
    expandParams = request
  where request = mkStripeRequest GET url params
        url     = "customers" </> getCustomerId customerid </> "subscriptions"
        params  =
          toExpandable expandParams
