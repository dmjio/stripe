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

import           Web.Stripe.Client.Internal (Method (DELETE), Stripe,
                                             StripeRequest (..), callAPI,
                                             (</>))
import           Web.Stripe.Types           (CustomerId (..),
                                             StripeDeleteResult (..),
                                             SubscriptionId (..))
import           Web.Stripe.Types.Util      (getCustomerId)

------------------------------------------------------------------------------
-- | Delete `Customer` `Discount` by `CustomerId`
deleteCustomerDiscount
    :: CustomerId -- ^ The Customer to remove the discount from
    -> Stripe StripeDeleteResult
deleteCustomerDiscount
    customerId = callAPI request
  where request = StripeRequest DELETE url params
        url     = "customers" </> getCustomerId customerId </> "discount"
        params  = []

------------------------------------------------------------------------------
-- | Delete `Subscription` `Discount` by `CustomerId` and `SubscriptionId`
deleteSubscriptionDiscount
  :: CustomerId     -- ^ The Customer to remove the discount from
  -> SubscriptionId -- ^ The Subscription to remove the discount from
  -> Stripe StripeDeleteResult
deleteSubscriptionDiscount
    customerId
    (SubscriptionId subId) = callAPI request
  where request = StripeRequest DELETE url params
        url     = "customers" </> getCustomerId customerId </> "subscriptions" </> subId </> "discount"
        params  = []
