module Web.Stripe.Subscriptions
    ( Subscription(..)
    , SubscriptionId(..)
    , createSubscription
    , getSubscription
    , updateSubscription
    , deleteSubscription
    ) where

import           Control.Applicative             ((<$>), (<*>), pure)
import           Data.Aeson
import           Data.Monoid
import           Data.Text                       (Text)
import           Data.Time
import           Web.Stripe.Client.Internal
import           Web.Stripe.Util
import           Web.Stripe.Types
import qualified Data.Text.Encoding as T

createSubscription :: CustomerId -> 
                      PlanId -> 
                      Stripe Subscription
createSubscription (CustomerId custId) (PlanId planId) = callAPI request
  where request = StripeRequest POST url params
        url     = "customers" </> custId </> "subscriptions"
        params  = [("plan", T.encodeUtf8 planId)]

getSubscription :: CustomerId ->
                   SubscriptionId ->
                   Stripe Subscription
getSubscription (CustomerId custId) (SubscriptionId subId) = callAPI request
  where request = StripeRequest GET url params
        url     = "customers" </> custId </> "subscriptions" </> subId
        params  = []

updateSubscription :: CustomerId ->
                      SubscriptionId ->
                      Stripe Subscription
updateSubscription (CustomerId custId) (SubscriptionId subId) = callAPI request 
  where request = StripeRequest POST url params
        url     = "customers" </> custId </> "subscriptions" </> subId
        params  = []

deleteSubscription :: CustomerId ->
                      SubscriptionId ->
                      Stripe Subscription
deleteSubscription (CustomerId custId) (SubscriptionId subId) = callAPI request 
  where request = StripeRequest DELETE url params
        url     = "customers" </> custId </> "subscriptions" </> subId
        params  = []

