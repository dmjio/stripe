{-# LANGUAGE OverloadedStrings #-}
module Web.Stripe.Balance
    ( -- * API
      getBalance
    , getBalanceTransaction
    , getBalanceTransactionHistory
      -- * Types
    , Balance       (..)
    , TransactionId (..)
    , StripeList    (..)
    , Limit
    , BalanceTransaction
    ) where

import           Web.Stripe.Client.Internal (Method (GET), Stripe,
                                             StripeRequest (..), callAPI,
                                             getParams, toText, (</>))
import           Web.Stripe.Types           (Balance, BalanceTransaction, Limit,
                                             StripeList, TransactionId (..))

------------------------------------------------------------------------------
-- | Retrieve the current 'Balance' for your Stripe account
getBalance :: Stripe Balance
getBalance = callAPI request
  where request = StripeRequest GET url params
        url     = "balance"
        params  = []

------------------------------------------------------------------------------
-- | Retrieve a 'BalanceTransaction' by 'TransactionId'
getBalanceTransaction
    :: TransactionId
    -> Stripe BalanceTransaction
getBalanceTransaction
    (TransactionId transactionId) = callAPI request
  where request = StripeRequest GET url params
        url     = "balance" </> "history" </> transactionId
        params  = []

------------------------------------------------------------------------------
-- | Retrieve the history of 'BalanceTransaction's
getBalanceTransactionHistory
    :: Maybe Limit
    -> Stripe (StripeList BalanceTransaction)
getBalanceTransactionHistory
    limit = callAPI request
  where request = StripeRequest GET url params
        url     = "balance" </> "history"
        params  = getParams [
                   ("limit", fmap toText limit)
                  ]

