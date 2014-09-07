{-# LANGUAGE OverloadedStrings #-}

module Web.Stripe.ApplicationFee
    ( -- * Application Fee Types
      ApplicationFee (..)
    , FeeId          (..)
    -- * API calls
    , getApplicationFee
    , getApplicationFees
    ) where

import           Web.Stripe.Client.Internal
import           Web.Stripe.Types

getApplicationFee 
    :: FeeId -- ^ The FeeID associated with the application
    -> Stripe ApplicationFee
getApplicationFee 
    (FeeId feeId) = callAPI request
  where request = StripeRequest GET url params
        url     = "application_fees" </> feeId
        params  = []

getApplicationFees :: Stripe (StripeList ApplicationFee)
getApplicationFees = callAPI request
  where request = StripeRequest GET url params
        url     = "application_fees"
        params  = []


