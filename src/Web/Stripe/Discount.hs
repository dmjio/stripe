{-# LANGUAGE OverloadedStrings #-}

module Web.Stripe.Discount where

import           Data.Monoid               ((<>))
import           Web.Stripe.Types          (Discount(..))


deleteCustomerDiscount :: CustomerId -> Stripe StripeDeleteResult
deleteCustomerDiscount (CustomerId customerId) = callAPI request
  where request = StripeRequest DELETE url params
        url     = "customers/" <> customerId <> "/discount"
        params  = []

deleteSubscriptionDiscount ::
  CustomerId -> 
  SubscriptionId -> 
  Stripe StripeDeleteResult
deleteSubscriptionDiscount (CustomerId customerId) (SubscriptionId subId) =
    callAPI request
  where request = StripeRequest DELETE url params
        params  = []
        url     = T.concat ["customers/"
                           , customerId
                           , "/subscriptions/"
                           , subId
                           , "/discount"
                           ]
