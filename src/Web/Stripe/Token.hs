{-# LANGUAGE OverloadedStrings #-}
module Web.Stripe.Token
   ( -- * API
     createCardToken
   , createBankAccountToken
   , getToken
     -- * Types
   , CardNumber    (..)
   , ExpMonth      (..)
   , ExpYear       (..)
   , CVC           (..)
   , Token         (..)
   , TokenId       (..)
   , Country       (..)
   , RoutingNumber (..)
   , AccountNumber (..)
   , Account       (..)
   ) where

import           Web.Stripe.Client.Internal (Method (GET, POST), Stripe,
                                             StripeRequest (..), callAPI,
                                             getParams, toText, (</>))
import           Web.Stripe.Types           (Account(..), AccountNumber (..),
                                             CVC (..), CardNumber (..),
                                             Country (..), ExpMonth (..),
                                             ExpYear (..), RoutingNumber (..),
                                             Token (..), TokenId (..))

------------------------------------------------------------------------------
-- | Create a `Token` by specifiy Credit `Card` information
createCardToken
    :: CardNumber -- ^ Credit Card Number
    -> ExpMonth   -- ^ Credit Card Expiration Month
    -> ExpYear    -- ^ Credit Card Expiration Year
    -> CVC        -- ^ Credit Card CVC
    -> Stripe Token
createCardToken
      (CardNumber number)
      (ExpMonth month)
      (ExpYear year)
      (CVC cvc)
          = callAPI request
  where request = StripeRequest POST url params
        url     = "tokens"
        params  = getParams [
                    ("card[number]", Just number)
                  , ("card[exp_month]", toText `fmap` Just month)
                  , ("card[exp_year]", toText `fmap` Just year)
                  , ("card[cvc]", Just cvc)
                  ]

------------------------------------------------------------------------------
-- | Create a `Token` for a specific BankAccount
createBankAccountToken
    :: Country        -- ^ Country of the TokenId to retrieve
    -> RoutingNumber  -- ^ Bank Account routing number
    -> AccountNumber  -- ^ Account Number
    -> Stripe Token
createBankAccountToken
    (Country country)
    (RoutingNumber routingNumber)
    (AccountNumber accountNumber)
    = callAPI request
  where request = StripeRequest POST url params
        url     = "tokens"
        params  = getParams [
                    ("bank_account[country]", Just country)
                  , ("bank_account[routing_number]", Just routingNumber)
                  , ("bank_account[account_number]", Just accountNumber)
                  ]

------------------------------------------------------------------------------
-- | Retrieve a `Token` by `TokenId`
getToken
    :: TokenId -- ^ The ID of the TokenId to retrieve
    -> Stripe Token
getToken (TokenId token) = callAPI request
  where request = StripeRequest GET url params
        url     = "tokens" </> token
        params  = []






