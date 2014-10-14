{-# LANGUAGE OverloadedStrings #-}
module Web.Stripe.Event
    ( -- * API
    --   getEvent
    -- , getEvents
    --   -- * Types
    -- , EventId    (..)
    -- , Event      (..)
    -- , StripeList (..)
    ) where

-- import           Web.Stripe.Client.Internal (Method (GET), Stripe, Stripe,
--                                              StripeRequest (..), callAPI, (</>), getParams, toText)
-- import           Web.Stripe.Types           (Event (..), EventId (..),
--                                              StripeList (..), Limit, StartingAfter, EndingBefore)

-- ------------------------------------------------------------------------------
-- -- | `Event` to retrieve by `EventId`
-- getEvent
--     :: EventId -- ^ The ID of the Event to retrieve
--     -> Stripe Event
-- getEvent (EventId eventid) = callAPI request
--   where request = StripeRequest GET url params
--         url     = "events" </> eventid
--         params  = []

-- ------------------------------------------------------------------------------
-- -- | `StripeList` of `Event`s to retrieve
-- getEvents
--     :: Maybe Limit              -- ^ Defaults to 10 if `Nothing` specified
--     -> StartingAfter EventId -- ^ Paginate starting after the following `EventId`
--     -> EndingBefore EventId  -- ^ Paginate ending before the following `EventId`
--     -> Stripe (StripeList Event)
-- getEvents 
--   limit
--   startingAfter
--   endingBefore  = callAPI request
--   where request = StripeRequest GET url params
--         url     = "events"
--         params  = getParams [
--             ("limit", toText `fmap` limit )
--           , ("starting_after", (\(EventId x) -> x) `fmap` startingAfter)
--           , ("ending_before", (\(EventId x) -> x) `fmap` endingBefore)
--           ]


