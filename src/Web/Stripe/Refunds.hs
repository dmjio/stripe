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

createRefund
    :: ChargeId
    -> Stripe Refund
createRefund
    (ChargeId chargeId) = callAPI request 
  where request = StripeRequest POST url params
        url     = "charges" </> chargeId </> "refunds"
        params  = []

getRefund
    :: ChargeId
    -> RefundId
    -> Stripe Refund
getRefund
    (ChargeId chargeId)
    (RefundId refId) = callAPI request 
   where request = StripeRequest GET url params
         url     = "charges" </> chargeId </> "refunds" </> refId
         params  = []

updateRefund
    :: ChargeId
    -> RefundId
    -> Stripe Refund
updateRefund
   (ChargeId chargeId)
   (RefundId refId) = callAPI request 
  where request = StripeRequest POST url params
        url     = "charges" </> chargeId </> "refunds" </> refId
        params  = []

getRefunds
    :: ChargeId
    -> Stripe (StripeList Refund)
getRefunds (ChargeId chargeId) = callAPI request 
  where request = StripeRequest GET url params
        url     = "charges" </> chargeId </> "refunds"
        params  = []

