{-# LANGUAGE OverloadedStrings #-}
module Web.Stripe.Subscriptions
    ( -- * Types
      Subscription   (..)
    , SubscriptionId (..)
      -- * API Calls
    , createSubscription
    , getSubscription
    , updateSubscription
    , deleteSubscription
    ) where


import           Control.Applicative
import           Data.Time

import           Web.Stripe.Client.Internal
import           Web.Stripe.Types

createSubscription
    :: CustomerId
    -> PlanId
    -> Stripe Subscription
createSubscription
    (CustomerId customerId) 
    (PlanId planId) = callAPI request
  where request = StripeRequest POST url params
        url     = "customers" </> customerId </> "subscriptions"
        params  = getParams [
                   ("plan", Just planId)
                  ]

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

