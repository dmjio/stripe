{-# LANGUAGE OverloadedStrings #-}
-------------------------------------------
-- |
-- Module      : Web.Stripe.Balance
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- < https:/\/\stripe.com/docs/api#balance >
--
-- @
-- import Web.Stripe         
-- import Web.Stripe.Balance (getBalance)
--
-- main :: IO ()
-- main = do
--   let config = SecretKey "secret_key"
--   result <- stripe config getBalance
--   case result of
--     Right balance    -> print balance 
--     Left stripeError -> print stripeError
-- @
module Web.Stripe.Balance
    ( -- * API
      getBalance
    , getBalanceTransaction
    , getBalanceTransactionExpandable
    , getBalanceTransactionHistory
      -- * Types
    , Balance                (..)
    , TransactionId          (..)
    , StripeList             (..)
    , EndingBefore
    , StartingAfter
    , Limit
    , BalanceTransaction
    , BalanceAmount
    ) where

import           Web.Stripe.Client.Internal (Method (GET), Stripe,
                                             StripeRequest (..), callAPI,
                                             getParams, toExpandable, toText,
                                             (</>))
import           Web.Stripe.Types           (Balance (..), BalanceAmount,
                                             BalanceTransaction, EndingBefore,
                                             ExpandParams, Limit, StartingAfter,
                                             StripeList (..),
                                             TransactionId (..))
import           Web.Stripe.Types.Util      (getTransactionId)

------------------------------------------------------------------------------
-- | Retrieve the current `Balance` for your Stripe account
getBalance :: Stripe Balance
getBalance = callAPI request
  where request = StripeRequest GET url params
        url     = "balance"
        params  = []

------------------------------------------------------------------------------
-- | Retrieve a 'BalanceTransaction' by 'TransactionId'
getBalanceTransaction
    :: TransactionId  -- ^ The `TransactionId` of the `Transaction` to retrieve
    -> Stripe BalanceTransaction
getBalanceTransaction
    transactionid = getBalanceTransactionExpandable transactionid []

------------------------------------------------------------------------------
-- | Retrieve a `BalanceTransaction` by `TransactionId` with `ExpandParams`
getBalanceTransactionExpandable
    :: TransactionId -- ^ The `TransactionId` of the `Transaction` to retrieve
    -> ExpandParams  -- ^ The `ExpandParams` of the object to be expanded
    -> Stripe BalanceTransaction
getBalanceTransactionExpandable
    transactionid expandParams = callAPI request
  where request = StripeRequest GET url params
        url     = "balance" </> "history" </> getTransactionId transactionid
        params  = toExpandable expandParams

------------------------------------------------------------------------------
-- | Retrieve the history of `BalanceTransaction`s
getBalanceTransactionHistory
    :: Limit                       -- ^ Defaults to 10 if `Nothing` specified
    -> StartingAfter TransactionId -- ^ Paginate starting after the following `TransactionId`
    -> EndingBefore TransactionId  -- ^ Paginate ending before the following `TransactionId`
    -> Stripe (StripeList BalanceTransaction)
getBalanceTransactionHistory
    limit
    startingAfter
    endingBefore = callAPI request
  where request = StripeRequest GET url params
        url     = "balance" </> "history"
        params  = getParams [
             ("limit", toText `fmap` limit )
           , ("starting_after", (\(TransactionId x) -> x) `fmap` startingAfter)
           , ("ending_before", (\(TransactionId x) -> x) `fmap` endingBefore)
           ]

