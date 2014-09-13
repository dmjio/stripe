{-# LANGUAGE OverloadedStrings #-}
module Web.Stripe.Refunds 
    ( -- * Refund Types
      Refund (..)
    , RefundId (..)
      -- * API Functions
    , createRefund
    , getRefund
    , getRefunds
    , updateRefund
    ) where

import           Control.Applicative
import           Data.Time

import           Web.Stripe.Client.Internal
import           Web.Stripe.Types

-- | <http://api.stripe.com/docs/api#refunds Refunds>

createRefund
    :: ChargeId -- ^ 'ChargeId' associated with the 'Charge' to be refunded
    -> Stripe Refund
createRefund
    (ChargeId chargeId) = callAPI request 
  where request = StripeRequest POST url params
        url     = "charges" </> chargeId </> "refunds"
        params  = []

getRefund
    :: ChargeId -- ^ 'ChargeId' associated with the 'Charge' to be retrieved
    -> RefundId -- ^ 'RefundId' associated with the 'Refund' to be retrieved
    -> Stripe Refund
getRefund
    (ChargeId chargeId)
    (RefundId refId) = callAPI request 
   where request = StripeRequest GET url params
         url     = "charges" </> chargeId </> "refunds" </> refId
         params  = []

updateRefund
    :: ChargeId -- ^ 'ChargeId' associated with the 'Charge' to be updated
    -> RefundId -- ^ 'RefundId' associated with the 'Refund' to be retrieved
    -> Stripe Refund
updateRefund
   (ChargeId chargeId)
   (RefundId refId) = callAPI request 
  where request = StripeRequest POST url params
        url     = "charges" </> chargeId </> "refunds" </> refId
        params  = []

getRefunds
    :: ChargeId  -- ^ 'ChargeId' associated with the 'Charge' to be updated
    -> Stripe (StripeList Refund)
getRefunds (ChargeId chargeId) = callAPI request 
  where request = StripeRequest GET url params
        url     = "charges" </> chargeId </> "refunds"
        params  = []

