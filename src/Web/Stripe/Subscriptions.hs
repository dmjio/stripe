module Web.Stripe.Subscriptions
    (  ) where

import           Control.Applicative             ((<$>), (<*>), pure)
import           Data.Aeson
import           Data.Monoid
import           Data.Text                       (Text)
import           Data.Time
import           Web.Stripe.Client.Internal
import           Web.Stripe.Util
import           Web.Stripe.Internal.StripeError
import qualified Data.Text.Encoding as T

newtype SubscriptionId = SubscriptionId Text deriving (Show, Eq)
newtype PlanId = PlanId Text deriving (Show, Eq)

data Subscription = Subscription {
      subscriptionId :: Text
} deriving (Show, Eq)

instance FromJSON Subscription where
   parseJSON (Object o) = Subscription <$> o .: "id"

createSubscription :: CustomerId -> PlanId -> Stripe Subscription
createSubscription (CustomerId custId) (PlanId planId) = callAPI request
  where request = StripeRequest POST url params
        url     = "customers/" <> custId <> "/subscriptions"
        params  = [("plan", T.encodeUtf8 planId)]

getSubscription :: CustomerId -> SubscriptionId -> Stripe Subscription
getSubscription (CustomerId custId) (SubscriptionId subId) = callAPI request
  where request = StripeRequest GET url params
        url     = "customers/" <> custId <> "/subscriptions/" <> subId
        params  = []

-- see parameters on this one
updateSubscription :: CustomerId -> SubscriptionId -> Stripe Subscription
updateSubscription (CustomerId custId) (SubscriptionId subId) = callAPI request 
  where request = StripeRequest POST url params
        url     = "customers/" <> custId <> "/subscriptions/" <> subId
        params  = []

deleteSubscription :: CustomerId -> SubscriptionId -> Stripe Subscription
deleteSubscription (CustomerId custId) (SubscriptionId subId) =
    callAPI request params
  where request = StripeRequest DELETE url
        url     = "customers/" <> custId <> "/subscriptions/" <> subId
        params  = []

