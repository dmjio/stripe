{-# LANGUAGE OverloadedStrings #-}

module Web.Stripe.Plan 
    ( -- * Plan Types
      PlanId   (..)
    , Plan     (..)
    , Interval (..)
      -- * API Functions
      ---- * Create Plan
    , createPlan
      ---- * Get Plan
    , getPlan
      ---- * Update Plan
    , updatePlan
      ---- * Delete Plan
    , deletePlan
    ) where

import           Control.Applicative             ((<$>), (<*>))
import           Data.Text                       (Text)
import qualified Data.Text.Encoding as T

import           Web.Stripe.Client.Internal
import           Web.Stripe.Types

------------------------------------------------------------------------------
-- | Create a 'Plan'
createPlan
    :: PlanId
    -> Amount
    -> Currency
    -> Interval
    -> Name
    -> Maybe IntervalCount
    -> Maybe TrialPeriodDays
    -> Maybe Description 
    -> Stripe Plan
createPlan 
    (PlanId planId)
    amount
    (Currency currency)
    interval
    name
    intervalCount
    trialPeriodDays
    description = callAPI request
  where request = StripeRequest POST url params
        url     = "plans"
        params = getParams [
                     ("id", Just planId) 
                   , ("amount", toText <$> Just amount) 
                   , ("currency", Just currency)
                   , ("interval", toText <$> Just interval) 
                   , ("name", Just name) 
                   , ("interval_count", (\(IntervalCount x) -> toText x) <$> intervalCount )
                   , ("trial_period_days", (\(TrialPeriodDays x) -> toText x) <$> trialPeriodDays )
                   , ("statement_description", description )
                 ]

------------------------------------------------------------------------------
-- | Get a 'Plan'
getPlan
    :: PlanId -- ^ The ID of the plan to retrieve
    -> Stripe Plan
getPlan
    (PlanId planId) = callAPI request
  where request = StripeRequest GET url params
        url     = "plans" </> planId
        params  = []

------------------------------------------------------------------------------
-- | Update a 'Plan'
updatePlan
    :: PlanId -- ^ The ID of the plan to update
    -> Maybe Name -- ^ The name of the Plan to update
    -> Maybe Description -- ^ The name of the Plan to update
    -> Stripe Plan
updatePlan
    (PlanId planId)
    name
    description = callAPI request 
  where request = StripeRequest POST url params
        url     = "plans" </> planId
        params  = getParams [
                      ("name", name)
                    , ("statement_description", description)
                  ]

------------------------------------------------------------------------------
-- | Delete a 'Plan'
deletePlan
    :: PlanId -- ^ The ID of the plan to delete
    -> Stripe StripeDeleteResult
deletePlan 
    (PlanId planId) = callAPI request 
  where request = StripeRequest DELETE url params
        url     = "plans" </> planId
        params  = []
