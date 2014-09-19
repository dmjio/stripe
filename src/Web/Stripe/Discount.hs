{-# LANGUAGE OverloadedStrings #-}
module Web.Stripe.Discount
    ( -- * API
      deleteCustomerDiscount
    , deleteSubscriptionDiscount
      -- * Types
    , StripeDeleteResult (..)
    , CustomerId         (..)
    , SubscriptionId     (..)
    ) where

import           Web.Stripe.Client.Internal (Method (GET, POST, DELETE), Stripe,
                                             StripeRequest (..), callAPI,
                                             getParams, toText, (</>))
import           Web.Stripe.Types           (CustomerId (..),
                                             StripeDeleteResult (..),
                                             SubscriptionId (..))

------------------------------------------------------------------------------
-- | Delete `Customer` `Discount` by `CustomerId`
deleteCustomerDiscount
    :: CustomerId -- ^ The Customer to remove the discount from
    -> Stripe StripeDeleteResult
deleteCustomerDiscount
    (CustomerId customerId) = callAPI request
  where request = StripeRequest DELETE url params
        url     = "customers" </> customerId </> "discount"
        params  = []

------------------------------------------------------------------------------
-- | Delete `Subscription` `Discount` by `CustomerId` and `SubscriptionId`
deleteSubscriptionDiscount
  :: CustomerId     -- ^ The Customer to remove the discount from
  -> SubscriptionId -- ^ The Subscription to remove the discount from
  -> Stripe StripeDeleteResult
deleteSubscriptionDiscount
    (CustomerId customerId)
    (SubscriptionId subId) = callAPI request
  where request = StripeRequest DELETE url params
        url     = "customers" </> customerId </> "subscriptions" </> subId </> "discount"
        params  = []
