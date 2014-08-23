module Web.Stripe.Token 
   ( -- * API Functions
   , createCardToken
   , createBankAccountToken
   , getToken
     -- * Types
   , Token(..)
   ) where

import           Data.Monoid
import           Web.Stripe.Client.Internal
import           Web.Stripe.Util
import           Web.Stripe.Types

config :: StripeConfig
config = StripeConfig "sk_test_zvqdM2SSA6WwySqM6KJQrqpH" "2014-03-28"

createCardToken :: Stripe Token
createCardToken = callAPI request 
  where request = StripeRequest POST url params
        url     = "tokens"
        params  = [ ("card[number]", "4242424242424242")
                  , ("card[exp_month]", "12")
                  , ("card[exp_year]", "2015")
                  , ("card[cvc]", "123")
                  ]

createBankAccountToken :: Stripe Token
createBankAccountToken = callAPI request 
  where request = StripeRequest POST url params
        url     = "tokens"
        params  = [ ("bank_account[country]", "US")
                  , ("bank_account[routing_number]", "110000000")
                  , ("bank_account[account_number]", "000123456789")
                  ]

getToken :: TokenId -> Stripe Token
getToken (TokenId token) = callAPI request
  where request = StripeRequest GET url params
        url     = "tokens/" <> token
        params  = []






