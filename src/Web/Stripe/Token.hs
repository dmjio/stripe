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
import           Web.Stripe.Util
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
        params  = [ ("card[number]", toBS number)
                  , ("card[exp_month]", toBS month)
                  , ("card[exp_year]", toBS year)
                  , ("card[cvc]", toBS cvc)
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






