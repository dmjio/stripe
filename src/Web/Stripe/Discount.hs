{-# LANGUAGE OverloadedStrings #-}

module Web.Stripe.Discount
    ( -- * Discount Types
      Discount   (..)
      -- * API Functions
    , deleteCustomerDiscount
    , deleteSubscriptionDiscount
    ) where

import           Web.Stripe.Client.Internal
import           Web.Stripe.Types

deleteCustomerDiscount :: 
    CustomerId -> -- ^ The Customer to remove the discount from
    Stripe StripeDeleteResult
deleteCustomerDiscount 
    (CustomerId customerId) = callAPI request
  where request = StripeRequest DELETE url params
        url     = "customers" </> customerId </> "discount"
        params  = []

deleteSubscriptionDiscount ::
  CustomerId ->     -- ^ The Customer to remove the discount from
  SubscriptionId -> -- ^ The Subscription to remove the discount from
  Stripe StripeDeleteResult
deleteSubscriptionDiscount 
    (CustomerId customerId) 
    (SubscriptionId subId) = callAPI request
  where request = StripeRequest DELETE url params
        url     = "customers" </> customerId </> "subscriptions" </> subId </> "discount"
        params  = []
