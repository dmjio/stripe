{-# LANGUAGE OverloadedStrings #-}
module Web.Stripe.Token
   ( -- * Types
     TokenId (..)
     -- * API Functions
   , createCardToken
   , createBankAccountToken
   , getToken
   ) where

import           Control.Applicative

import           Web.Stripe.Client.Internal
import           Web.Stripe.Types

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
      (CVC cvc) = callAPI request
  where request = StripeRequest POST url params
        url     = "tokens"
        params  = getParams [
                    ("card[number]", toText <$> Just number)
                  , ("card[exp_month]", toText <$> Just month)
                  , ("card[exp_year]", toText <$> Just year)
                  , ("card[cvc]", toText <$> Just cvc)
                  ]

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

getToken
    :: TokenId -- ^ The ID of the TokenId to retrieve
    -> Stripe Token
getToken (TokenId token) = callAPI request
  where request = StripeRequest GET url params
        url     = "tokens" </> token
        params  = []






