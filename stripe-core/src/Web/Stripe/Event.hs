{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
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
      GetEvent
    , getEvent
    , GetEvents
    , getEvents
      -- * Types
    , Created    (..)
    , EventId    (..)
    , Event      (..)
    , EventData  (..)
    , EventType  (..)
    , StripeList (..)
    , Limit
    ) where

import           Web.Stripe.StripeRequest (Method (GET, POST, DELETE), Param(..),
                                           StripeHasParam, StripeRequest (..),
                                           StripeReturn, ToStripeParam(..),
                                           mkStripeRequest)
import           Web.Stripe.Util     ((</>), getParams, toText)
import           Web.Stripe.Types           (Created(..), Event (..), EventId (..), Limit, EventData(..),
                                             EventType(..), StripeList (..), Limit,
                                             StartingAfter, EndingBefore)

------------------------------------------------------------------------------
-- | `Event` to retrieve by `EventId`
data GetEvent
type instance StripeReturn GetEvent = Event
getEvent
    :: EventId -- ^ The ID of the Event to retrieve
    -> StripeRequest GetEvent
getEvent (EventId eventid) = request
  where request = mkStripeRequest GET url params
        url     = "events" </> eventid
        params  = []

------------------------------------------------------------------------------
-- | `StripeList` of `Event`s to retrieve
data GetEvents
type instance StripeReturn GetEvents = (StripeList Event)
instance StripeHasParam GetEvents Created
instance StripeHasParam GetEvents (EndingBefore EventId)
instance StripeHasParam GetEvents Limit
instance StripeHasParam GetEvents (StartingAfter EventId)
-- instance StripeHasParam GetEvents EventType -- FIXME
getEvents
    :: StripeRequest GetEvents
getEvents
                = request
  where request = mkStripeRequest GET url params
        url     = "events"
        params  = []
