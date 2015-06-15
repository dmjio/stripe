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
-- {-\# LANGUAGE OverloadedStrings \#-}
-- import Web.Stripe
-- import Web.Stripe.Balance (getBalance)
--
-- main :: IO ()
-- main = do
--   let config = StripeConfig (StripeKey "secret_key")
--   result <- stripe config getBalance
--   case result of
--     Right balance    -> print balance
--     Left stripeError -> print stripeError
-- @
module Web.Stripe.Balance
    ( -- * API
      GetBalance
    , getBalance
    , GetBalanceTransaction
    , getBalanceTransaction
    , GetBalanceTransactionHistory
    , getBalanceTransactionHistory
      -- * Types
    , AvailableOn            (..)
    , Balance                (..)
    , BalanceAmount          (..)
    , BalanceTransaction     (..)
    , Created                (..)
    , Currency               (..)
    , EndingBefore           (..)
    , ExpandParams           (..)
    , Limit                  (..)
    , Source                 (..)
    , StartingAfter          (..)
    , StripeList             (..)
    , TimeRange              (..)
    , TransactionId          (..)
    , TransactionType        (..)
    ) where

import           Web.Stripe.StripeRequest (Method (GET), StripeHasParam,
                                           StripeRequest (..),
                                           StripeReturn, ToStripeParam(..),
                                           mkStripeRequest)
import           Web.Stripe.Util          ((</>))
import           Web.Stripe.Types         (AvailableOn(..), Balance (..),
                                           BalanceAmount(..), BalanceTransaction(..),
                                           Created(..), Currency(..),
                                           EndingBefore(..), ExpandParams(..),
                                           Limit(..), Source(..), StartingAfter(..),
                                           StripeList (..), TimeRange(..),
                                           TransferId(..), TransactionId (..),
                                           TransactionType(..))
import           Web.Stripe.Types.Util    (getTransactionId)

------------------------------------------------------------------------------
-- | Retrieve the current `Balance` for your Stripe account
getBalance :: StripeRequest GetBalance
getBalance = request
  where request = mkStripeRequest GET url params
        url     = "balance"
        params  = []

data GetBalance
type instance StripeReturn GetBalance = Balance

------------------------------------------------------------------------------
-- | Retrieve a 'BalanceTransaction' by 'TransactionId'
getBalanceTransaction
    :: TransactionId  -- ^ The `TransactionId` of the `Transaction` to retrieve
    -> StripeRequest GetBalanceTransaction
getBalanceTransaction
    transactionid = request
  where request = mkStripeRequest GET url params
        url     = "balance" </> "history" </> getTransactionId transactionid
        params  = []

data GetBalanceTransaction
type instance StripeReturn GetBalanceTransaction = BalanceTransaction
instance StripeHasParam GetBalanceTransaction ExpandParams

------------------------------------------------------------------------------
-- | Retrieve the history of `BalanceTransaction`s
getBalanceTransactionHistory
    :: StripeRequest GetBalanceTransactionHistory
getBalanceTransactionHistory
                = request
  where request = mkStripeRequest GET url params
        url     = "balance" </> "history"
        params  = []

data GetBalanceTransactionHistory
type instance StripeReturn GetBalanceTransactionHistory = (StripeList BalanceTransaction)
instance StripeHasParam GetBalanceTransactionHistory AvailableOn
instance StripeHasParam GetBalanceTransactionHistory (TimeRange AvailableOn)
instance StripeHasParam GetBalanceTransactionHistory Created
instance StripeHasParam GetBalanceTransactionHistory (TimeRange Created)
instance StripeHasParam GetBalanceTransactionHistory Currency
instance StripeHasParam GetBalanceTransactionHistory (EndingBefore TransactionId)
instance StripeHasParam GetBalanceTransactionHistory Limit
instance StripeHasParam GetBalanceTransactionHistory (StartingAfter TransactionId)
instance (ToStripeParam a) => StripeHasParam GetBalanceTransactionHistory (Source a)
instance StripeHasParam GetBalanceTransactionHistory TransferId
instance StripeHasParam GetBalanceTransactionHistory TransactionType
