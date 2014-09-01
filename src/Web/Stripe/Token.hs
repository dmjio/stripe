module Web.Stripe.Token 
   ( -- * API Functions
     createCardToken
   , createBankAccountToken
   , getToken
     -- * Types
   , Token(..)
   ) where

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
      (CVC cvc) =  callAPI request 
  where request = StripeRequest POST url params
        url     = "tokens"
        params  = getParams [ 
                    ("card[number]", Just $ toText number)
                  , ("card[exp_month]", Just $ toText month)
                  , ("card[exp_year]", Just $ toText year)
                  , ("card[cvc]", Just $ toText cvc)
                  ]

createBankAccountToken 
    :: Country        -- ^ Country of the Token to retrieve
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
    :: TokenId -- ^ The ID of the Token to retrieve
    -> Stripe Token
getToken (TokenId token) = callAPI request
  where request = StripeRequest GET url params
        url     = "tokens" </> token
        params  = []






