{-# LANGUAGE OverloadedStrings #-}
-------------------------------------------
-- |
-- Module      : Web.Stripe.Event
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- < https:/\/\stripe.com/docs/api#events >
--
-- @
-- import Web.Stripe         
-- import Web.Stripe.Event
--
-- main :: IO ()
-- main = do
--   let config = SecretKey "secret_key"
--   result <- stripe config $ getEvents Nothing Nothing Nothing
--   case result of
--     Right events     -> print events
--     Left stripeError -> print stripeError
-- @
module Web.Stripe.Event
    ( -- * API
      getEvent
    , getEvents
      -- * Types
    , EventId    (..)
    , Event      (..)
    , EventData  (..)
    , EventType  (..)
    , StripeList (..)
    , Limit
    ) where

import           Web.Stripe.Client.Internal (Method (GET), Stripe, Stripe, 
                                             StripeRequest (..), callAPI, (</>), getParams, toText)
import           Web.Stripe.Types           (Event (..), EventId (..), Limit, EventData(..),
                                             EventType(..), StripeList (..), Limit,
                                             StartingAfter, EndingBefore)

------------------------------------------------------------------------------
-- | `Event` to retrieve by `EventId`
getEvent
    :: EventId -- ^ The ID of the Event to retrieve
    -> Stripe Event
getEvent (EventId eventid) = callAPI request
  where request = StripeRequest GET url params
        url     = "events" </> eventid
        params  = []

------------------------------------------------------------------------------
-- | `StripeList` of `Event`s to retrieve
getEvents
    :: Maybe Limit           -- ^ Defaults to 10 if `Nothing` specified
    -> StartingAfter EventId -- ^ Paginate starting after the following `EventId`
    -> EndingBefore EventId  -- ^ Paginate ending before the following `EventId`
    -> Stripe (StripeList Event)
getEvents 
  limit
  startingAfter
  endingBefore  = callAPI request
  where request = StripeRequest GET url params
        url     = "events"
        params  = getParams [
            ("limit", toText `fmap` limit )
          , ("starting_after", (\(EventId x) -> x) `fmap` startingAfter)
          , ("ending_before", (\(EventId x) -> x) `fmap` endingBefore)
          ]


