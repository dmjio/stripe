{-# LANGUAGE OverloadedStrings #-}
module Web.Stripe.Subscriptions
    ( -- * Types
      Subscription   (..)
    , SubscriptionId (..)
      -- * API 
    , createSubscription
    , getSubscription
    , updateSubscription
    , deleteSubscription
    ) where

import           Web.Stripe.Client.Internal
import           Web.Stripe.Types

------------------------------------------------------------------------------
-- | Create a `Subscription` by `CustomerId` and `PlanId`
createSubscription
    :: CustomerId
    -> PlanId
    -> Stripe Subscription
createSubscription
    (CustomerId customerId) 
    (PlanId planId) = callAPI request
  where request = StripeRequest POST url params
        url     = "customers" </> customerId </> "subscriptions"
        params  = getParams [ ("plan", Just planId)  ]

------------------------------------------------------------------------------
-- | Retrieve a `Subscription` by `CustomerId` and `SubscriptionId`
getSubscription 
    :: CustomerId 
    -> SubscriptionId 
    -> Stripe Subscription
getSubscription 
    (CustomerId customerId) 
    (SubscriptionId subscriptionId) = callAPI request
  where request = StripeRequest GET url params
        url     = "customers" </> customerId </> "subscriptions" </> subscriptionId
        params  = []

------------------------------------------------------------------------------
-- | Update a `Subscription` by `CustomerId` and `SubscriptionId`
updateSubscription 
    :: CustomerId 
    -> SubscriptionId 
    -> Stripe Subscription
updateSubscription
    (CustomerId customerId)
    (SubscriptionId subscriptionId) = callAPI request 
  where request = StripeRequest POST url params
        url     = "customers" </> customerId </> "subscriptions" </> subscriptionId
        params  = []

------------------------------------------------------------------------------
-- | Delete a `Subscription` by `CustomerId` and `SubscriptionId`
deleteSubscription
    :: CustomerId
    -> SubscriptionId 
    -> Stripe Subscription
deleteSubscription
    (CustomerId customerId)
    (SubscriptionId subscriptionId) = callAPI request 
  where request = StripeRequest DELETE url params
        url     = "customers" </> customerId </> "subscriptions" </> subscriptionId
        params  = []

