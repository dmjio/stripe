-------------------------------------------
-- |
-- Module      : Web.Stripe
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- < https:/\/\stripe.com/docs/api >
--
-- @
-- import Web.Stripe
-- import Web.Stripe.Account
--
-- main :: IO ()
-- main = do
--   let config = StripeConfig "secret_key"
--   result <- stripe config getAccountDetails
--   case result of
--     Right details -> print details
--     Left stripeError -> print stripeError
-- @
module Web.Stripe (
    module Web.Stripe.Client
  , module Web.Stripe.StripeRequest
  , stripe
  , showAmount
  ) where

import Web.Stripe.Client (StripeConfig(..), StripeKey(..))
import Web.Stripe.StripeRequest
import Web.Stripe.Types (showAmount)
import Web.Stripe.Client.Stripe (stripe)

