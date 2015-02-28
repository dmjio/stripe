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
    , BitcoinReceiver      (..)
    , BitcoinReceiverId    (..)
    , BitcoinTransaction   (..)
    , BitcoinTransactionId (..)
    , Email                (..)
    , StripeList           (..)
    ) where

import           Web.Stripe.Client.Internal ( Method (GET, POST)
                                            , Stripe
                                            , StripeRequest (..)
                                            , (</>)
                                            , callAPI
                                            , getParams
                                            , toText )
import           Web.Stripe.Types

------------------------------------------------------------------------------
-- | Retrieve the object that represents your Stripe account
createReceiver
 :: Integer -- ^ Amount
 -> Email   -- ^ Email
 -> Stripe BitcoinReceiver
createReceiver amount (Email email) = callAPI request
  where request = StripeRequest POST url params
        url     = "bitcoin/receivers"
        params  = getParams [ ("currency", Just "usd")
                            , ("amount", Just $ toText amount )
                            , ("email", Just email )
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
  :: Maybe Limit          -- ^ Defaults to 10 if `Nothing` specified
  -> StartingAfter BitcoinReceiverId  -- ^ Paginate starting after the following `BitcoinReceiverId`
  -> EndingBefore BitcoinReceiverId  -- ^ Paginate ending before the following `BitcoinReceiverId`
  -> Stripe (StripeList BitcoinReceiver)
listReceivers
  limit
  startingAfter
  endingBefore  = callAPI request
  where
    request = StripeRequest GET url params
    url     = "bitcoin/receivers"
    params  = getParams [
            ("limit", toText `fmap` limit )
          , ("starting_after", (\(BitcoinReceiverId x) -> x) `fmap` startingAfter)
          , ("ending_before", (\(BitcoinReceiverId x) -> x) `fmap` endingBefore)
          ]
