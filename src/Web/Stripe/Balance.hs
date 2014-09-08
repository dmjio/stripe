{-# LANGUAGE OverloadedStrings #-}

module Web.Stripe.Balance
    ( -- * Balance Types
      Balance       (..)
    , TransactionId (..)
      -- * API calls
    , getBalance
    , getBalanceTransaction
    , getBalanceTransactionHistory
    ) where

import           Control.Applicative

import           Web.Stripe.Client.Internal
import           Web.Stripe.Types

getBalance :: Stripe Balance
getBalance = callAPI req 
  where req    = StripeRequest GET url params
        url    = "balance"
        params = []

getBalanceTransaction
    :: TransactionId
    -> Stripe BalanceTransaction
getBalanceTransaction
    (TransactionId transactionId) = callAPI request 
  where request = StripeRequest GET url params
        url     = "balance" </> "history" </> transactionId
        params  = []
   
getBalanceTransactionHistory
    :: Maybe Limit 
    -> Stripe (StripeList BalanceTransaction)
getBalanceTransactionHistory
    limit = callAPI request 
  where request = StripeRequest GET url params
        url     = "balance" </> "history"
        params  = getParams [ 
                   ("limit", toText <$> limit) 
                  ]
                        
