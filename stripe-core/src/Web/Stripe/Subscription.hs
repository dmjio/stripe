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
-- {-\# LANGUAGE OverloadedStrings \#-}
-- import Web.Stripe
-- import Web.Stripe.Subscription
-- import Web.Stripe.Customer
-- import Web.Stripe.Plan
--
-- main :: IO ()
-- main = do
--   let config = StripeConfig (StripeKey "secret_key")
--   result <- stripe config $ createCustomer
--   case result of
--     (Left stripeError) -> print stripeError
--     (Right (Customer { customerId = cid })) -> do
--       result <- stripe config $ createPlan (PlanId "free plan")
--                                            (Amount 0)
--                                            USD
--                                            Month
--                                            (PlanName "sample plan")
--       case result of
--         (Left stripeError) -> print stripeError
--         (Right (Plan { planId = pid })) -> do
--            result <- stripe config $ createSubscription cid pid
--            case result of
--              (Left stripeError)   -> print stripeError
--              (Right subscription) -> print subscription
-- @
module Web.Stripe.Subscription
    ( -- * API
      CreateSubscription
    , createSubscription
    , GetSubscription
    , getSubscription
    , UpdateSubscription
    , updateSubscription
    , CancelSubscription
    , cancelSubscription
    , GetSubscriptions
    , getSubscriptions
      -- * Types
    , ApplicationFeePercent (..)
    , AtPeriodEnd        (..)
    , CustomerId         (..)
    , CouponId           (..)
    , Coupon             (..)
    , EndingBefore       (..)
    , ExpandParams       (..)
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
    , TaxPercent         (..)
    , TrialEnd           (..)
    ) where

import           Web.Stripe.StripeRequest   (Method (GET, POST, DELETE),
                                             StripeHasParam, StripeReturn,
                                             StripeRequest (..),
                                             ToStripeParam(..), mkStripeRequest)
import           Web.Stripe.Util            ((</>))
import           Web.Stripe.Types           (ApplicationFeePercent(..),
                                             AtPeriodEnd(..), CardId(..),
                                             CustomerId (..), Coupon(..),
                                             CouponId(..), EndingBefore(..),
                                             ExpandParams(..), Limit(..), MetaData(..),
                                             PlanId (..), Prorate(..), Quantity(..),
                                             StartingAfter(..),
                                             Subscription (..), StripeList(..),
                                             SubscriptionId (..),
                                             SubscriptionStatus (..), TaxPercent(..), 
                                             TrialEnd(..))
import           Web.Stripe.Types.Util      (getCustomerId)

------------------------------------------------------------------------------
-- | Create a `Subscription` by `CustomerId` and `PlanId`
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

data CreateSubscription
type instance StripeReturn CreateSubscription = Subscription
instance StripeHasParam CreateSubscription CouponId
instance StripeHasParam CreateSubscription Prorate
instance StripeHasParam CreateSubscription TrialEnd
instance StripeHasParam CreateSubscription CardId
instance StripeHasParam CreateSubscription Quantity
instance StripeHasParam CreateSubscription ApplicationFeePercent
instance StripeHasParam CreateSubscription MetaData
instance StripeHasParam CreateSubscription TaxPercent

------------------------------------------------------------------------------
-- | Retrieve a `Subscription` by `CustomerId` and `SubscriptionId`
getSubscription
    :: CustomerId       -- ^ The `CustomerId` of the `Subscription`
    -> SubscriptionId   -- ^ The `SubscriptionId` of the `Subscription` to retrieve
    -> StripeRequest GetSubscription
getSubscription
    customerid
    (SubscriptionId subscriptionid)
                = request
  where request = mkStripeRequest GET url params
        url     = "customers" </> getCustomerId customerid </>
                  "subscriptions" </> subscriptionid
        params  = []

data GetSubscription
type instance StripeReturn GetSubscription = Subscription
instance StripeHasParam GetSubscription ExpandParams

------------------------------------------------------------------------------
-- | Update a `Subscription` by `CustomerId` and `SubscriptionId`
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
instance StripeHasParam UpdateSubscription TaxPercent

------------------------------------------------------------------------------
-- | Delete a `Subscription` by `CustomerId` and `SubscriptionId`
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

data CancelSubscription
instance StripeHasParam CancelSubscription AtPeriodEnd
type instance StripeReturn CancelSubscription = Subscription

------------------------------------------------------------------------------
-- | Retrieve all active `Subscription`s
getSubscriptions
    :: StripeRequest GetSubscriptions
getSubscriptions
    = request
  where request = mkStripeRequest GET url params
        url     = "subscriptions"
        params  = []

data GetSubscriptions
type instance StripeReturn GetSubscriptions = StripeList Subscription
instance StripeHasParam GetSubscriptions ExpandParams
instance StripeHasParam GetSubscriptions (EndingBefore SubscriptionId)
instance StripeHasParam GetSubscriptions Limit
instance StripeHasParam GetSubscriptions (StartingAfter SubscriptionId)
