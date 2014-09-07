{-# LANGUAGE OverloadedStrings #-}

module Web.Stripe.Account
    ( -- * Account Types
      AccountId (..)
    , Account   (..)
      -- * API calls
    , getAccountDetails
    ) where

import           Web.Stripe.Client.Internal
import           Web.Stripe.Types          

-- | Retrieve the object that represents your Stripe account
getAccountDetails :: Stripe Account
getAccountDetails = callAPI request
  where request = StripeRequest GET url params
        url     = "account"
        params  = []
