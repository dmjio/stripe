{-# LANGUAGE OverloadedStrings #-}
-------------------------------------------
-- |
-- Module      : Web.Stripe.Bitcoin
-- Copyright   : (c) David Johnson, 2015
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- < https://stripe.com/docs/api#bitcoin_receivers >
-- < https://stripe.com/docs/guides/bitcoin >
--
-- @
-- import Web.Stripe         
-- import Web.Stripe.Bitcoin
--
-- main :: IO ()
-- main = do
--   let config = SecretKey "secret_key"
--   result <- stripe config listAllReceivers
--   case result of
--     Right receivers  -> print receivers
--     Left stripeError -> print stripeError
-- @
module Web.Stripe.Bitcoin
    ( -- * API
      createReceiver
    , getReceiver
    , listReceivers
      -- * Types
    , BitcoinReceiver (..)
    ) where

import           Web.Stripe.Client.Internal ( Method (GET, POST)
                                            , Stripe
                                            , StripeRequest (..)
                                            , (</>)
                                            , callAPI )
import           Web.Stripe.Types

------------------------------------------------------------------------------
-- | Retrieve the object that represents your Stripe account
createReceiver :: Stripe BitcoinReceiver
createReceiver = callAPI request
  where request = StripeRequest POST url params
        url     = "bitcoin/receivers"
        params  = []

------------------------------------------------------------------------------
-- | Retrieve a `BitcoinReceiver`
getReceiver
 :: BitcoinReceiverId
 -> Stripe BitcoinReceiver
getReceiver (BitcoinReceiverId receiverId) = callAPI request
  where request = StripeRequest GET url params
        url     = "bitcoin/receivers" </> receiverId
        params  = []

------------------------------------------------------------------------------
-- | Retrieve a list of `BitcoinReceiver`s
listReceivers
 :: BitcoinReceiverId
 -> Stripe (StripeList BitcoinReceiver)
listReceivers (BitcoinReceiverId receiverId) = callAPI request
  where request = StripeRequest GET url params
        url     = "bitcoin/receivers" </> receiverId
        params  = []
