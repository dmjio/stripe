{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
-------------------------------------------
-- |
-- Module      : Web.Stripe.Token
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : code@dmj.io
-- Stability   : experimental
-- Portability : POSIX
--
-- < https:/\/\stripe.com/docs/api#tokens >
--
-- @
-- {-\# LANGUAGE OverloadedStrings \#-}
-- import Web.Stripe
-- import Web.Stripe.Token
--
-- main :: IO ()
-- main = do
--   let config = StripeConfig (StripeKey "secret_key")
--       credit = CardNumber "4242424242424242"
--       em  = ExpMonth 12
--       ey  = ExpYear 2015
--       cvc = CVC "123"
--       cardinfo = (mkNewCard credit em ey) { newCardCVC = Just cvc }
--   result <- stripe config $ createCardToken (Just cardinfo)
--   case result of
--     Right token -> print token
--     Left stripeError -> print stripeError
-- @
module Web.Stripe.Token
   ( -- * API
     CreateCardToken
   , createCardToken
   , CreateBankAccountToken
   , createBankAccountToken
   , GetCardToken
   , getCardToken
   , GetBankAccountToken
   , getBankAccountToken
     -- * Types
   , Account        (..)
   , AccountNumber  (..)
   , BankAccount    (..)
   , Card           (..)
   , CardNumber     (..)
   , Country        (..)
   , CustomerId     (..)
   , CVC            (..)
   , ExpMonth       (..)
   , ExpYear        (..)
   , NewBankAccount (..)
   , mkNewCard
   , NewCard        (..)
   , RoutingNumber  (..)
   , Token          (..)
   , TokenId        (..)
   , TokenType      (..)
   ) where

import           Web.Stripe.StripeRequest (Method (GET, POST),
                                           StripeHasParam, StripeRequest (..),
                                           StripeReturn, ToStripeParam(..),
                                           mkStripeRequest)
import           Web.Stripe.Util          ((</>))
import           Web.Stripe.Types         (Account(..), AccountNumber (..),
                                           CVC (..), CardNumber (..), CustomerId(..),
                                           Country (..), ExpMonth (..),
                                           BankAccount(..), ExpYear (..),
                                           NewBankAccount(..), mkNewCard,
                                           NewCard(..), RoutingNumber (..),
                                           Card(..), Token (..),
                                           TokenId (..), TokenType(..)
                                          )

------------------------------------------------------------------------------
-- | Create a `Token` by specifiying Credit `Card` information
createCardToken
    :: Maybe NewCard -- ^ optional `NewCard` data
    -> StripeRequest CreateCardToken
createCardToken
  newCard
                = request
  where request = mkStripeRequest POST url params
        url     = "tokens"
        params  = maybe id toStripeParam newCard $ []

data CreateCardToken
type instance StripeReturn CreateCardToken = Token Card
instance StripeHasParam CreateCardToken CustomerId

------------------------------------------------------------------------------
-- | Create a `Token` for a specific `BankAccount`
createBankAccountToken
    :: Maybe NewBankAccount -- ^ option `BankAccount` information
    -> StripeRequest CreateBankAccountToken
createBankAccountToken
  newBankAccount
                = request
  where request = mkStripeRequest POST url params
        url     = "tokens"
        params  = maybe id toStripeParam newBankAccount $ []

data CreateBankAccountToken
type instance StripeReturn CreateBankAccountToken = Token BankAccount

------------------------------------------------------------------------------
-- | Retrieve a `Token` by `TokenId`
getCardToken
    :: TokenId -- ^ The `TokenId` of the `Card` `Token` to retrieve
    -> StripeRequest GetCardToken
getCardToken (TokenId token) = request
  where request = mkStripeRequest GET url params
        url     = "tokens" </> token
        params  = []

data GetCardToken
type instance StripeReturn GetCardToken = Token Card

------------------------------------------------------------------------------
-- | Retrieve a `Token` by `TokenId`
getBankAccountToken
    :: TokenId -- ^ The `TokenId` of the `BankAccount` `Token` to retrieve
    -> StripeRequest GetBankAccountToken
getBankAccountToken (TokenId token) = request
  where request = mkStripeRequest GET url params
        url     = "tokens" </> token
        params  = []

data GetBankAccountToken
type instance StripeReturn GetBankAccountToken = Token BankAccount
