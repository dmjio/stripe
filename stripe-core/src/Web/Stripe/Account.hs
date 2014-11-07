{-# LANGUAGE OverloadedStrings #-}
-------------------------------------------
-- |
-- Module      : Web.Stripe.Account
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- < https:/\/\stripe.com/docs/api#account >
--
-- @
-- import Web.Stripe         
-- import Web.Stripe.Account
--
-- main :: IO ()
-- main = do
--   let config = SecretKey "secret_key"
--   result <- stripe config getAccountDetails
--   case result of
--     Right account    -> print account
--     Left stripeError -> print stripeError
-- @
module Web.Stripe.Account
    ( -- * API
      getAccountDetails
      -- * Types
    , Account   (..)
    , AccountId (..)
    ) where

import           Web.Stripe.Client.Types    ( Method (GET)
                                            , StripeRequest (..) )
import           Web.Stripe.Types           ( Account   (..)
                                            , AccountId (..) )

------------------------------------------------------------------------------
-- | Retrieve the object that represents your Stripe account
getAccountDetails :: StripeRequest Account
getAccountDetails = request
  where request = StripeRequest GET url params
        url     = "account"
        params  = []
