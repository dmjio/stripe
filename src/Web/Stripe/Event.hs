{-# LANGUAGE OverloadedStrings #-}
module Web.Stripe.Event
    ( -- * API
      getEvent
    , getEvents
      -- * Types
    , EventId    (..)
    , Event      (..)
    , StripeList (..)
    ) where

import           Web.Stripe.Client.Internal (Method (GET), Stripe, Stripe,
                                             StripeRequest (..), callAPI, (</>))
import           Web.Stripe.Types           (Event (..), EventId (..),
                                             StripeList (..))

------------------------------------------------------------------------------
-- | `Event` to retrieve by `EventId`
getEvent
    :: EventId -- ^ The ID of the Event to retrieve
    -> Stripe Event
getEvent (EventId eventId) = callAPI request
  where request = StripeRequest GET url params
        url     = "events" </> eventId
        params  = []

------------------------------------------------------------------------------
-- | `StripeList` of `Event`s to retrieve
getEvents :: Stripe (StripeList Event)
getEvents = callAPI request
  where request = StripeRequest GET url params
        url     = "events"
        params  = []

