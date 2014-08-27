module Web.Stripe.Token 
   ( -- * API Functions
     createCardToken
   , createBankAccountToken
   , getToken
     -- * Types
   , Token(..)
   ) where

import           Data.Monoid
import qualified Data.Text.Encoding as T
import           Web.Stripe.Client.Internal
import           Web.Stripe.Types

createCardToken :: 
    CardNumber -> -- ^ Credit Card Number
    ExpMonth   -> -- ^ Credit Card Expiration Month
    ExpYear    -> -- ^ Credit Card Expiration Year
    CVC        -> -- ^ Credit Card CVC 
    Stripe Token
createCardToken 
      (CardNumber number)
      (ExpMonth month)
      (ExpYear year)
      (CVC cvc) =  callAPI request 
  where request = StripeRequest POST url params
        url     = "tokens"
        params  = [ ("card[number]", Just $ toText number)
                  , ("card[exp_month]", Just $ toText month)
                  , ("card[exp_year]", Just $ toText year)
                  , ("card[cvc]", Just $ toText cvc)
                  ]

createBankAccountToken :: 
    (Country country) ->
    (RoutingNumber routingNumber) ->
    (AccountNumber number) ->
    Stripe Token
createBankAccountToken = callAPI request 
  where request = StripeRequest POST url params
        url     = "tokens"
        params  = getParams [ 
                    ("bank_account[country]", country)
                  , ("bank_account[routing_number]", routingNumber)
                  , ("bank_account[account_number]", accountNumber)
                  ]

getToken :: TokenId -> Stripe Token
getToken (TokenId token) = callAPI request
  where request = StripeRequest GET url params
        url     = "tokens" </> token
        params  = []






