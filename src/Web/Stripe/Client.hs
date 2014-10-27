-- |
-- Module      : Web.Stripe.Client
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Web.Stripe.Client
    ( -- * Execute a `Stripe` action
      stripe
      -- * `Stripe` Monad
    , Stripe
      -- * `Stripe` Secret Key 
    , StripeConfig       (..)
      -- * Error type for Request
    , module Web.Stripe.Client.Error
    ) where

import Web.Stripe.Client.Internal
import Web.Stripe.Client.Error


