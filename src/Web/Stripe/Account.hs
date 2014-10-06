{-# LANGUAGE OverloadedStrings #-}
module Web.Stripe.Account
    ( -- * API
      getAccountDetails
      -- * Types
    , Account   (..)
    ) where

import           Web.Stripe.Client.Internal ( Method (GET)
                                            , Stripe
                                            , StripeRequest (..)
                                            , callAPI )
import           Web.Stripe.Types           ( Account(..) )

------------------------------------------------------------------------------
-- | Retrieve the object that represents your Stripe account
getAccountDetails :: Stripe Account
getAccountDetails = callAPI request
  where request = StripeRequest GET url params
        url     = "account"
        params  = []
