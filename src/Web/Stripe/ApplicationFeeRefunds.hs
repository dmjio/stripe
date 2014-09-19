{-# LANGUAGE OverloadedStrings #-}
module Web.Stripe.ApplicationFeeRefunds
    ( -- * API
      createApplicationFeeRefund
    , getApplicationFeeRefund
    , getApplicationFeeRefunds
      -- * Types
    , FeeId                  (..)
    , RefundId               (..)
    , ApplicationFee         (..)
    , ApplicationFeeRefund   (..)        
    , StripeList             (..)
    , Amount
    ) where

import           Web.Stripe.Client.Internal (Method (POST, GET), Stripe,
                                             StripeRequest (..), callAPI,
                                             getParams, toText, (</>))
import           Web.Stripe.Types           (Amount, ApplicationFee,
                                             ApplicationFeeRefund(..), FeeId (..),
                                             RefundId (..), StripeList)

------------------------------------------------------------------------------
-- | Create a new 'ApplicationFeeRefund'
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
                   ("amount", fmap toText amount)
                  ]

------------------------------------------------------------------------------
-- | Retrieve an existing 'ApplicationFeeRefund'
getApplicationFeeRefund
    :: FeeId
    -> RefundId
    -> Stripe ApplicationFeeRefund
getApplicationFeeRefund (FeeId feeId) (RefundId refundId)
    = callAPI request
  where request = StripeRequest GET url params
        url     = "application_fees" </> feeId </> "refunds" </> refundId
        params  = []

------------------------------------------------------------------------------
-- | Retrieve a list of all 'ApplicationFeeRefund's for a given Application 'FeeId'
getApplicationFeeRefunds
    :: FeeId
    -> Stripe (StripeList ApplicationFeeRefund)
getApplicationFeeRefunds (FeeId feeId)
    = callAPI request
  where
    request = StripeRequest GET url params
    url     = "application_fees" </> feeId </> "refunds"
    params  = []

