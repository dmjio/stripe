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

import           Data.Aeson
import           Web.Stripe.Client.Internal ( Method (GET, POST)
                                            , Stripe
                                            , StripeRequest (..)
                                            , (</>)
                                            , callAPI )
import           Web.Stripe.Types

------------------------------------------------------------------------------
-- | Retrieve the object that represents your Stripe account
createReceiver
 :: Integer -- Amount
 -> Currency 
 -> Stripe BitcoinReceiver
createReceiver x b = callAPI request
  where request = StripeRequest POST url params
        url     = "bitcoin/receivers"
        params  = [ ("currency", "usd")
                  , ("amount", "100")
                  , ("email", "djohnson.m@gmail.com")
                  ]

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
 :: Stripe Value
listReceivers = callAPI request
  where request = StripeRequest GET url params
        url     = "bitcoin/receivers"
        params  = []
