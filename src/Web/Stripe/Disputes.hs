{-# LANGUAGE OverloadedStrings #-}
module Web.Stripe.Disputes
    ( -- * API
      updateDispute
    , closeDispute
      -- * Types
    , ChargeId      (..)
    , Dispute       (..)
    , DisputeReason (..)
    , DisputeStatus (..)
    , Evidence      (..)
    ) where

import           Web.Stripe.Client.Internal (Method (POST), Stripe,
                                             StripeRequest (..), callAPI,
                                             getParams, (</>))
import           Web.Stripe.Types           (ChargeId (..), Dispute (..),
                                             DisputeReason (..),
                                             DisputeStatus (..), Evidence (..))

------------------------------------------------------------------------------
-- | `Dispute` to be updated
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
                   ("evidence", (\(Evidence x) -> x) `fmap` evidence)
                  ]
------------------------------------------------------------------------------
-- | `Dispute` to be closed
closeDispute
    :: ChargeId  -- ^ The ID of the Charge being disputed
    -> Stripe Dispute
closeDispute
    (ChargeId chargeId) = callAPI request
  where request = StripeRequest POST url params
        url     = "charges" </> chargeId </> "dispute" </> "close"
        params  = []
