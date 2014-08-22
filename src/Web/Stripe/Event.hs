module Web.Stripe.Event
    ( 
    ) where

import           Control.Applicative             ((<$>), (<*>))
import           Data.Aeson
import           Data.Monoid
import           Data.Text                       (Text)
import           Data.Time
import           Web.Stripe.Client.Internal
import           Web.Stripe.Util
import           Web.Stripe.Internal.StripeError

config :: StripeConfig
config = StripeConfig "sk_test_zvqdM2SSA6WwySqM6KJQrqpH" "2014-03-28"

newtype EventId = EventId Text deriving (Show, Eq)

data EventType = ChargeEvent
   | RefundEvent
   | AdjustmentEvent
   | ApplicationFeeEvent
   | ApplicationFeeRefundEvent
   | TransferEvent
   | TransferFailureEvent
      deriving (Show, Eq)

data Event = Event { 
      eventId :: EventId
    , eventCreated :: UTCTime
    , eventType :: Text
} deriving (Show)

instance FromJSON Event where
   parseJSON (Object o) = undefined

getEvent :: EventId -> Stripe Event
getEvent (EventId eventId) = callAPI request
  where request = StripeRequest GET url params
        url     = "events/" <> eventId
        params  = []

getEvents :: Stripe [Event]
getEvents = callAPI request
  where request = StripeRequest GET "events" params
        params  = []

