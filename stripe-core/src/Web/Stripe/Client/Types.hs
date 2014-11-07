{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Web.Stripe.Client.Types
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Web.Stripe.Client.Types
  ( -- * Types
    Method(..)
  , StripeRequest (..)
  , StripeConfig  (..)
  , APIVersion    (..)
  ) where

import           Data.ByteString            (ByteString)
import           Data.Text                  (Text)

------------------------------------------------------------------------------
-- | HTTP Method
--
-- The other methods are not required by the Stripe API
data Method
  = DELETE
  | GET
  | POST
    deriving (Eq, Ord, Read, Show)

------------------------------------------------------------------------------
-- | HTTP Params
type Params = [(ByteString, ByteString)]

------------------------------------------------------------------------------
-- | Stripe Request holding `Method`, URL and `Params` for a Request
data StripeRequest a = StripeRequest
    { method      :: Method -- ^ Method of StripeRequest (i.e. `GET`, `PUT`, `POST`, `PUT`)
    , endpoint    :: Text   -- ^ Endpoint of StripeRequest
    , queryParams :: Params -- ^ Query Parameters of StripeRequest
    } deriving Show

------------------------------------------------------------------------------
-- | Stripe secret key
data StripeConfig = StripeConfig
    { secretKey :: ByteString
    } deriving Show

------------------------------------------------------------------------------
-- | API Version
data APIVersion =
    V20141007 -- ^ Stripe API Version for this package release
    deriving Eq

instance Show APIVersion where
    show V20141007 = "2014-10-07"
