module Web.Stripe.Event
    ( -- * Event Types
      EventId (..)
    , Event   (..)
      -- * API calls
    , getEvent
    , getEvents
    ) where

import           Control.Applicative             ((<$>), (<*>))
import           Web.Stripe.Client.Internal
import           Web.Stripe.Types

getEvent 
    :: EventId -- ^ The ID of the Event to retrieve
    -> Stripe Event
getEvent (EventId eventId) = callAPI request
  where request = StripeRequest GET url params
        url     = "events" </> eventId
        params  = []

getEvents :: Stripe (StripeList Event)
getEvents = callAPI request
  where request = StripeRequest GET url params
        url     = "events"
        params  = []

