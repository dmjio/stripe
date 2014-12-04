{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
-------------------------------------------
-- |
-- Module      : Web.Stripe.Token
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- < https:/\/\stripe.com/docs/api#tokens >
--
-- @
-- import Web.Stripe
-- import Web.Stripe.Token
--
-- main :: IO ()
-- main = do
--   let config = SecretKey "secret_key"
--       credit = CardNumber "4242424242424242"
--       em  = ExpMonth 12
--       ey  = ExpYear 2015
--       cvc = CVC "123"
--   result <- stripe config $ createCardToken cn em ey cvc
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
   , CardNumber    (..)
   , ExpMonth      (..)
   , ExpYear       (..)
   , CVC           (..)
   , Token         (..)
   , TokenId       (..)
   , TokenType     (..)
   , Country       (..)
   , RoutingNumber (..)
   , AccountNumber (..)
   , Account       (..)
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
                                           NewBankAccount(..), NewCard(..),
                                           RoutingNumber (..), Card(..),
                                           Token (..), TokenId (..), TokenType(..),
                                          )

------------------------------------------------------------------------------
-- | Create a `Token` by specifiying Credit `Card` information
data CreateCardToken
type instance StripeReturn CreateCardToken = Token Card
instance StripeHasParam CreateCardToken CustomerId
createCardToken
    :: Maybe NewCard
    -> StripeRequest CreateCardToken
createCardToken
  newCard
                = request
  where request = mkStripeRequest POST url params
        url     = "tokens"
        params  = maybe id toStripeParam newCard $ []

------------------------------------------------------------------------------
-- | Create a `Token` for a specific `BankAccount`
data CreateBankAccountToken
type instance StripeReturn CreateBankAccountToken = Token BankAccount
createBankAccountToken
    :: NewBankAccount
    -> StripeRequest CreateBankAccountToken
createBankAccountToken
  newBankAccount
                = request
  where request = mkStripeRequest POST url params
        url     = "tokens"
        params  = toStripeParam newBankAccount []

------------------------------------------------------------------------------
-- | Retrieve a `Token` by `TokenId`
data GetCardToken
type instance StripeReturn GetCardToken = Token Card
getCardToken
    :: TokenId -- ^ The `TokenId` of the `Card` `Token` to retrieve
    -> StripeRequest GetCardToken
getCardToken (TokenId token) = request
  where request = mkStripeRequest GET url params
        url     = "tokens" </> token
        params  = []

------------------------------------------------------------------------------
-- | Retrieve a `Token` by `TokenId`
data GetBankAccountToken
type instance StripeReturn GetBankAccountToken = Token BankAccount
getBankAccountToken
    :: TokenId -- ^ The `TokenId` of the `BankAccount` `Token` to retrieve
    -> StripeRequest GetBankAccountToken
getBankAccountToken (TokenId token) = request
  where request = mkStripeRequest GET url params
        url     = "tokens" </> token
        params  = []
