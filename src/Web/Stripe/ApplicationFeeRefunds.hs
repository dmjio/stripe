{-# LANGUAGE OverloadedStrings #-}

module Web.Stripe.ApplicationFeeRefunds
    ( -- * Application Fee Refund Types
      FeeId                  (..)
    , ApplicationFee         (..)
    , ApplicationFeeRefundId (..)
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
    :: FeeId        -- ^ The FeeID associated with the application
    -> Maybe Amount -- ^ The Amount associated with the Fee (optional)
    -> Stripe ApplicationFeeRefund
createApplicationFeeRefund
    (FeeId feeId) 
    amount      = callAPI request
  where request = StripeRequest POST url params
        url     = "application_fees" </> feeId </> "refunds"
        params  = getParams [ 
                   ("amount", toText <$> amount) 
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

