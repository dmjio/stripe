{-# LANGUAGE OverloadedStrings #-}

module Web.Stripe.Balance
    ( Balance (..)
    , TransactionId (..)
    , getBalance
    , getBalanceTransaction
    ) where

import           Data.Monoid
import           Web.Stripe.Client.Internal
import           Web.Stripe.Types

config :: StripeConfig
config = StripeConfig "sk_test_zvqdM2SSA6WwySqM6KJQrqpHss" "2014-03-28"

getBalance :: Stripe Balance
getBalance = callAPI req 
  where req    = StripeRequest GET url params
        url    = "balance"
        params = []

getBalanceTransaction ::TransactionId -> Stripe Balance
getBalanceTransaction (TransactionId tId) = callAPI request 
  where request = StripeRequest GET url params
        url     = "balance/history/" <> tId
        params  = []
                              
