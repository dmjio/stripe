{-# LANGUAGE OverloadedStrings #-}
module Web.Stripe.ApplicationFeeRefunds
    ( -- * Types
      ApplicationFee (..)
    , ApplicationFeeRefundId (..)
    , FeeId (..)
      -- * API calls
    , createApplicationFeeRefund
    , retrieveApplicationFeeRefund
    , getApplicationFeeRefunds
    ) where

import           Web.Stripe.Client.Internal
import           Web.Stripe.Types
import           Control.Applicative

-- | Create a new application refund
createApplicationFeeRefund 
    :: FeeId 
    -> Maybe Amount 
    -> Stripe ApplicationFeeRefund
createApplicationFeeRefund (FeeId feeId) amount 
    = callAPI request
  where request = StripeRequest POST url params
        url     = "application_fees" </> feeId </> "refunds"
        params  = getParams [ 
                   ("amount", (\(Amount x) -> toText x) <$> amount) 
                  ]

-- | Create an existing application refund
retrieveApplicationFeeRefund 
    :: FeeId
    -> RefundId
    -> Stripe ApplicationFeeRefund
retrieveApplicationFeeRefund (FeeId feeId) (RefundId refundId)
    = callAPI request
  where request = StripeRequest GET url params
        url     = "application_fees" </> feeId </> "refunds" </> refundId
        params  = []

-- | Get a list of all application fee refunds
getApplicationFeeRefunds 
    :: FeeId 
    -> Stripe (StripeList ApplicationFeeRefund)
getApplicationFeeRefunds (FeeId feeId) 
    = callAPI request
  where
    request = StripeRequest GET url params
    url     = "application_fees" </> feeId </> "refunds" 
    params  = []

