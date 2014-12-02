{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
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
      GetBalance
    , getBalance
{-
    , getBalanceTransaction
    , getBalanceTransactionExpandable
    , getBalanceTransactionHistory
-}
      -- * Types
    , Balance                (..)
    , TransactionId          (..)
    , StripeList             (..)
    , EndingBefore           (..)
    , StartingAfter          (..)
    , Limit                  (..)
    , BalanceTransaction     (..)
    , BalanceAmount          (..)
    ) where

import           Web.Stripe.StripeRequest (Method (GET, POST, DELETE), Param(..),
                                           StripeHasParam, StripeRequest (..),
                                           StripeReturn, ToStripeParam(..),
                                           mkStripeRequest)
import           Web.Stripe.Util          (toExpandable, (</>))
import           Web.Stripe.Types         (Balance (..), BalanceAmount,
                                           BalanceTransaction, Created(..), Currency(..), EndingBefore(..),
                                           ExpandParams, Limit(..), StartingAfter(..),
                                           StripeList (..),
                                           TransferId(..), TransactionId (..))
import           Web.Stripe.Types.Util      (getTransactionId)

------------------------------------------------------------------------------
-- | Retrieve the current `Balance` for your Stripe account
data GetBalance
type instance StripeReturn GetBalance = Balance
getBalance :: StripeRequest GetBalance
getBalance = request
  where request = mkStripeRequest GET url params
        url     = "balance"
        params  = []

------------------------------------------------------------------------------
-- | Retrieve a 'BalanceTransaction' by 'TransactionId'
data GetBalanceTransaction
type instance StripeReturn GetBalanceTransaction = BalanceTransaction
getBalanceTransaction
    :: TransactionId  -- ^ The `TransactionId` of the `Transaction` to retrieve
    -> StripeRequest GetBalanceTransaction
getBalanceTransaction
    transactionid = getBalanceTransactionExpandable transactionid []

------------------------------------------------------------------------------
-- | Retrieve a `BalanceTransaction` by `TransactionId` with `ExpandParams`
getBalanceTransactionExpandable
    :: TransactionId -- ^ The `TransactionId` of the `Transaction` to retrieve
    -> ExpandParams  -- ^ The `ExpandParams` of the object to be expanded
    -> StripeRequest GetBalanceTransaction
getBalanceTransactionExpandable
    transactionid expandParams = request
  where request = mkStripeRequest GET url params
        url     = "balance" </> "history" </> getTransactionId transactionid
        params  = toExpandable expandParams

------------------------------------------------------------------------------
-- | Retrieve the history of `BalanceTransaction`s
data GetBalanceTransactionHistory
type instance StripeReturn GetBalanceTransactionHistory = (StripeList BalanceTransaction)
-- instance StripeHasParam GetBalanceTransactionHistory AvailableOn -- FIXME
instance StripeHasParam GetBalanceTransactionHistory Created
instance StripeHasParam GetBalanceTransactionHistory Currency
instance StripeHasParam GetBalanceTransactionHistory (EndingBefore TransactionId)
instance StripeHasParam GetBalanceTransactionHistory Limit
instance StripeHasParam GetBalanceTransactionHistory (StartingAfter TransactionId)
-- instance StripeHasParam GetBalanceTransactionHistory Source -- FIXME
instance StripeHasParam GetBalanceTransactionHistory TransferId
-- instance StripeHasParam GetBalanceTransactionHistory TransactionType -- FIXME

getBalanceTransactionHistory
    :: StripeRequest GetBalanceTransactionHistory
getBalanceTransactionHistory
                = request
  where request = mkStripeRequest GET url params
        url     = "balance" </> "history"
        params  = []

