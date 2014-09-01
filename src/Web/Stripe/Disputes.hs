{-# LANGUAGE OverloadedStrings #-}

module Web.Stripe.Disputes
    ( -- * Discount Types
      Dispute       (..)
    , DisputeReason (..)
    , DisputeStatus (..)
    , Evidence      (..)
      -- * API Functions
    , updateDispute
    , closeDispute
    ) where

import           Control.Applicative        ((<$>))
import           Web.Stripe.Client.Internal
import           Web.Stripe.Types

updateDispute
    :: ChargeId        -- ^ The ID of the Charge being disputed
    -> Maybe Evidence  -- ^ Text-only evidence of the dispute
    -> Stripe Dispute
updateDispute
    (ChargeId chargeId)
    evidence
    = callAPI request
  where request = StripeRequest POST url params
        url     = "charges" </> chargeId </> "dispute"
        params  = getParams [ 
                   ("evidence", (\(Evidence x) -> x) <$> evidence) 
                  ]

closeDispute
    :: ChargeId  -- ^ The ID of the Charge being disputed
    -> Stripe Dispute
closeDispute
    (ChargeId chargeId) = callAPI request
  where request = StripeRequest POST url params
        url     = "charges" </> chargeId </> "dispute" </> "close"
        params  = []
