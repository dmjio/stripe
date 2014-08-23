{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Stripe.Customer
    ( -- * Types
      Customer(..)
    , CustomerId(..)
      -- * API Calls
    , createCustomer
    , updateCustomer
    , deleteCustomer
    , getCustomer
    ) where

import           Data.Aeson
import           Data.Monoid                     ((<>))
import           Web.Stripe.Client.Internal
import           Web.Stripe.Types
import           Web.Stripe.Util

config :: StripeConfig
config = StripeConfig "sk_test_zvqdM2SSA6WwySqM6KJQrqpH" "2014-03-28"

createCustomer :: Stripe Customer
createCustomer = callAPI req
  where req = StripeRequest POST "customers" params
        params = [ (x, y) | (x, Just y) <- [
                     ("account_balance", fmap toBS customerAccountBalanceOptions)
                   , ("description", fmap toBS customerDescriptonOptions)
                   , ("email", fmap toBS customerEmailOptions)
                   , ("quantity", fmap toBS customerQuantityOptions)
                   , ("trial_end", fmap toBS customerTrialEndOptions)
                   ]
                 ]

getCustomer :: CustomerId -> Stripe Customer
getCustomer (CustomerId cid) = callAPI request 
  where request = StripeRequest GET url params
        url     = "customers/" <> cid
        params  = []

updateCustomer :: CustomerId -> Stripe Customer
updateCustomer options (CustomerId cid) = callAPI request 
  where request = StripeRequest POST url params
        url     = "customers/" <> cid
        params  = []

deleteCustomer :: CustomerId ->  Stripe Customer
deleteCustomer options (CustomerId cid) = callAPI request 
  where request = StripeRequest DELETE url params
        url     = "customers/" <> cid
        params  = []

