-------------------------------------------
-- |
-- Module      : Web.Stripe.Stripe
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
-- 
-- @
-- import Web.Stripe         
-- import Web.Stripe.Account (getAccountDetails)
--
-- main :: IO ()
-- main = do
--   let config = SecretKey "secret_key"
--   result <- stripe config getAccountDetails
--   case result of
--     Right AccountDetails {..} -> print accountId
--     Left stripeError          -> print stripeError
-- @
-- < https:/\/\stripe.com/docs/api >

module Web.Stripe (
    module Web.Stripe.Client 
  ) where

import Web.Stripe.Client
