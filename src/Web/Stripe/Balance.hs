{-# LANGUAGE OverloadedStrings #-}
module Web.Stripe.Balance
    ( Balance       (..)
    , TransactionId (..)
    , getBalance
    , getBalanceTransaction
    , getBalanceTransactionHistory
    ) where

import           Web.Stripe.Client.Internal
import           Web.Stripe.Types

getBalance :: Stripe Balance
getBalance = callAPI req 
  where req    = StripeRequest GET url params
        url    = "balance"
        params = []

getBalanceTransaction :: TransactionId -> Stripe BalanceTransaction
getBalanceTransaction (TransactionId transactionId) 
    = callAPI request 
  where request = StripeRequest GET url params
        url     = "balance" </> "history" </> transactionId
        params  = []
   
getBalanceTransactionHistory :: Stripe (StripeList BalanceTransaction)
getBalanceTransactionHistory
    = callAPI request 
  where request = StripeRequest GET url params
        url     = "balance" </> "history"
        params  = []
                        
