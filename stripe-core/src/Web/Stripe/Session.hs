{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
-------------------------------------------
-- |
-- Module      : Web.Stripe.Session
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Web.Stripe.Session
    ( -- * API
      CreateSession
    , createSession
    , GetSession
    , getSession
      -- * Types
    , Amount       (..)
    , Charge       (..)
    , ChargeId     (..)
    , EndingBefore (..)
    , ExpandParams (..)
    , Session       (..)
    , SessionId     (..)
    , StripeList   (..)
    ) where

import           Web.Stripe.StripeRequest   (Method (GET, POST),
                                             StripeHasParam, StripeReturn,
                                             StripeRequest (..), toStripeParam, mkStripeRequest)
import           Web.Stripe.Util            ((</>))
import           Web.Stripe.Types           (Amount(..), Charge (..), ChargeId (..), Currency(..),
                                             EndingBefore(..), Limit(..),
                                             MetaData(..), Session (..),
                                             SessionId (..),
                                             StartingAfter(..), ExpandParams(..),
                                             StripeList (..))

------------------------------------------------------------------------------
-- | create a `Session`
createSession
    :: Amount
    -> Currency
    -> StripeRequest CreateSession
createSession
    amount
    currency    = request
  where request = mkStripeRequest POST url params
        url     = "checkout" </> "sessions"
        params  = toStripeParam amount $
                  toStripeParam currency $
                  []

data CreateSession
type instance StripeReturn CreateSession = Session

------------------------------------------------------------------------------
-- | Retrieve a `Session` by `ChargeId` and `SessionId`
getSession
    :: SessionId -- ^ `SessionId` associated with the `Session` to be retrieved
    -> StripeRequest GetSession
getSession
   (SessionId sessionid) = request
   where request = mkStripeRequest GET url params
         url     = "checkout" </> "sessions" </> sessionid
         params  = []

data GetSession
type instance StripeReturn GetSession = Session
instance StripeHasParam GetSession ExpandParams
