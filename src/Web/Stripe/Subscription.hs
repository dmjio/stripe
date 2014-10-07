{-# LANGUAGE OverloadedStrings #-}
module Web.Stripe.Subscription
    ( -- * Types
      CustomerId     (..)
    , PlanId         (..)
    , Subscription   (..)
    , SubscriptionId (..)
      -- * API
    , createSubscription
    , getSubscription
    , getSubscriptions
    , updateSubscription
    , deleteSubscription
    ) where

import           Web.Stripe.Client.Internal (Method (GET, POST, DELETE), Stripe,
                                             StripeRequest (..), callAPI, toText, toMetaData,
                                             getParams, (</>))
import           Web.Stripe.Types           (CustomerId (..), PlanId (..),
                                             Subscription (..), Limit, StartingAfter, EndingBefore,
                                             SubscriptionId (..), MetaData)

------------------------------------------------------------------------------
-- | Create a `Subscription` by `CustomerId` and `PlanId`
createSubscription
    :: CustomerId
    -> PlanId
    -> MetaData
    -> Stripe Subscription
createSubscription
    (CustomerId customerId)
    (PlanId planId) 
    metadata    = callAPI request
  where request = StripeRequest POST url params
        url     = "customers" </> customerId </> "subscriptions"
        params  = toMetaData metadata ++ getParams [ ("plan", Just planId)  ]

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
-- | Retrieve active `Subscription`s
getSubscriptions
    :: CustomerId
    -> Limit                        -- ^ Defaults to 10 if `Nothing` specified
    -> StartingAfter SubscriptionId -- ^ Paginate starting after the following `CustomerId`
    -> EndingBefore SubscriptionId  -- ^ Paginate ending before the following `CustomerId`
    -> Stripe Subscription
getSubscriptions
    (CustomerId customerId)
    limit
    startingAfter
    endingBefore
     = callAPI request
  where request = StripeRequest GET url params
        url     = "customers" </> customerId </> "subscriptions"
        params  = getParams [
            ("limit", toText `fmap` limit )
          , ("starting_after", (\(SubscriptionId x) -> x) `fmap` startingAfter)
          , ("ending_before", (\(SubscriptionId x) -> x) `fmap` endingBefore)
          ]

------------------------------------------------------------------------------
-- | Update a `Subscription` by `CustomerId` and `SubscriptionId`
updateSubscription
    :: CustomerId
    -> SubscriptionId
    -> MetaData
    -> Stripe Subscription
updateSubscription
    (CustomerId customerId)
    (SubscriptionId subscriptionId)
    metadata    = callAPI request
  where request = StripeRequest POST url params
        url     = "customers" </> customerId </> "subscriptions" </> subscriptionId
        params  = toMetaData metadata

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

