{-# LANGUAGE OverloadedStrings #-}
module Web.Stripe.Refunds 
    ( -- * API 
      createRefund
    , getRefund
    , getRefunds
    , updateRefund
      -- * Types
    , Refund     (..)
    , RefundId   (..)
    , ChargeId   (..)
    , StripeList (..)
    ) where

import           Web.Stripe.Client.Internal
import           Web.Stripe.Types

-- | <http://api.stripe.com/docs/api#refunds Refunds>

------------------------------------------------------------------------------
-- | `Refund` a `Charge`
createRefund
    :: ChargeId -- ^ 'ChargeId' associated with the 'Charge' to be refunded
    -> Stripe Refund
createRefund
    (ChargeId chargeId) = callAPI request 
  where request = StripeRequest POST url params
        url     = "charges" </> chargeId </> "refunds"
        params  = []

------------------------------------------------------------------------------
-- | Retrieve a `Refund` by `ChargeId` and `RefundId`
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

------------------------------------------------------------------------------
-- | Update a `Refund` by `ChargeId` and `RefundId`
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

------------------------------------------------------------------------------
-- | Retrieve a lot of Refunds by `ChargeId`
getRefunds
    :: ChargeId  -- ^ 'ChargeId' associated with the 'Charge' to be updated
    -> Stripe (StripeList Refund)
getRefunds (ChargeId chargeId) = callAPI request 
  where request = StripeRequest GET url params
        url     = "charges" </> chargeId </> "refunds"
        params  = []

