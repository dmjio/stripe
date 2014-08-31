{-# LANGUAGE OverloadedStrings #-}
module Web.Stripe.ApplicationFee
    ( ApplicationFee(..)
    , FeeId(..)
    , getApplicationFee
    ) where

import           Web.Stripe.Client.Internal
import           Web.Stripe.Types

getApplicationFee :: FeeId -> Stripe ApplicationFee
getApplicationFee (FeeId feeId) = callAPI request
  where request = StripeRequest GET url params
        url     = "application_fees" </> feeId
        params  = []

getApplicationFees :: Stripe (StripeList ApplicationFee)
getApplicationFees = callAPI request
  where request = StripeRequest GET url params
        url     = "application_fees"
        params  = []


