{-# LANGUAGE OverloadedStrings #-}
-------------------------------------------
-- |
-- Module      : Web.Stripe.Dispute
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- < https:/\/\stripe.com/docs/api#diputes >
--
-- @
-- import Web.Stripe
-- import Web.Stripe.Charge
-- import Web.Stripe.Dispute
--
-- main :: IO ()
-- main = do
--   let config = SecretKey "secret_key"
--   result <- stripe config $ do
--     Charge { chargeDispute = dispute } <- getCharge (ChargeId "charge_id")
--     return dispute
--   case result of
--     Right (Just dispute) -> print dispute
--     Right Nothing        -> print "no dispute on this charge"
--     Left  stripeError    -> print stripeError
-- @
module Web.Stripe.Dispute
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

import           Web.Stripe.Client.Types    (Method (POST), StripeRequest (..))
import           Web.Stripe.Client.Util     (getParams, (</>), toMetaData)
import           Web.Stripe.Types           (ChargeId (..), Dispute (..),
                                             DisputeReason (..),
                                             DisputeStatus (..),
                                             Evidence (..), MetaData)
import           Web.Stripe.Types.Util      (getChargeId)

------------------------------------------------------------------------------
-- | `Dispute` to be updated
updateDispute
    :: ChargeId        -- ^ The ID of the Charge being disputed
    -> Maybe Evidence  -- ^ Text-only evidence of the dispute
    -> MetaData        -- ^ `MetaData` associated with `Dispute`
    -> StripeRequest Dispute
updateDispute
    chargeId
    evidence
    metadata    = request
  where request = StripeRequest POST url params
        url     = "charges" </> getChargeId chargeId </> "dispute"
        params  = toMetaData metadata ++ getParams [
                   ("evidence", (\(Evidence x) -> x) `fmap` evidence)
                  ]
------------------------------------------------------------------------------
-- | `Dispute` to be closed
closeDispute
    :: ChargeId  -- ^ The ID of the Charge being disputed
    -> StripeRequest Dispute
closeDispute
    chargeId = request
  where request = StripeRequest POST url params
        url     = "charges" </> getChargeId chargeId </> "dispute" </> "close"
        params  = []
