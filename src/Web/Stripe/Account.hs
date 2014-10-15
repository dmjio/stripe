{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Web.Stripe.Account
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Web.Stripe.Account
    ( -- * API
      getAccountDetails
      -- * Types
    , Account   (..)
    , AccountId (..)
    ) where

import           Web.Stripe.Client.Internal ( Method (GET)
                                            , Stripe
                                            , StripeRequest (..)
                                            , callAPI )
import           Web.Stripe.Types           ( Account   (..)
                                            , AccountId (..) )

------------------------------------------------------------------------------
-- | Retrieve the object that represents your Stripe account
getAccountDetails :: Stripe Account
getAccountDetails = callAPI request
  where request = StripeRequest GET url params
        url     = "account"
        params  = []
