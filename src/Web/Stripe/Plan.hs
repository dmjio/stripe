{-# LANGUAGE OverloadedStrings #-}

module Web.Stripe.Plan 
    ( -- * API Functions
      ---- * Create Plan
      createPlan
    , createPlanBase
      ---- * Get Plan
    , getPlan
      ---- * Update Plan
    , updatePlanName
    , updatePlanDescription
    , updatePlanBase
      ---- * Delete Plan
    , deletePlan
      -- * Plan Types
    , PlanId   (..)
    , Plan     (..)
    , Interval (..)
    ) where

import           Control.Applicative             ((<$>), (<*>))
import           Data.Text                       (Text)
import qualified Data.Text.Encoding as T

import           Web.Stripe.Client.Internal
import           Web.Stripe.Types

------------------------------------------------------------------------------
-- | Base Request for creating a 'Plan', useful for making custom `Plan` creation requests
createPlanBase
    :: PlanId                -- ^ Unique string used to identify `Plan`
    -> Amount                -- ^ Positive integer in cents representing how much to charge on a recurring basis
    -> Currency              -- ^ `Currency` of `Plan`
    -> Interval              -- ^ Billing Frequency (i.e. `Day`, `Week` or `Month`)
    -> Name                  -- ^ Name of `Plan` to be displayed on `Invoice`s
    -> Maybe IntervalCount   -- ^ Number of intervals between each `Subscription` billing, default 1
    -> Maybe TrialPeriodDays -- ^ Integer number of days a trial will have
    -> Maybe Description     -- ^ An arbitrary string to be displayed on `Customer` credit card statements
    -> Stripe Plan
createPlanBase
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
-- | Create a `Plan`
createPlan 
    :: PlanId   -- ^ Unique string used to identify `Plan`
    -> Amount   -- ^ Positive integer in cents (or 0 for a free plan) representing how much to charge on a recurring basis
    -> Currency -- ^ `Currency` of `Plan`
    -> Interval -- ^ Billing Frequency 
    -> Name     -- ^ Name of `Plan` to be displayed on `Invoice`s
    -> Stripe Plan
createPlan 
    planId
    amount
    currency
    interval
    name = createPlanBase planId amount 
              currency interval name
              Nothing Nothing Nothing

------------------------------------------------------------------------------
-- | Create a `Plan` with a specified `IntervalCount`
createPlanIntervalCount
    :: PlanId        -- ^ Unique string used to identify `Plan`
    -> Amount        -- ^ Positive integer in cents (or 0 for a free plan) representing how much to charge on a recurring basis
    -> Currency      -- ^ `Currency` of `Plan`
    -> Interval      -- ^ Billing Frequency 
    -> Name          -- ^ Name of `Plan` to be displayed on `Invoice`s
    -> IntervalCount -- ^ Name of `Plan` to be displayed on `Invoice`s
    -> Stripe Plan
createPlanIntervalCount
    planId
    amount
    currency
    interval
    name
    intervalCount = createPlanBase planId amount 
                    currency interval name
                    (Just intervalCount) Nothing Nothing

------------------------------------------------------------------------------
-- | Create a `Plan` with a specified number of `TrialPeriodDays`
createPlanTrialPeriodDays
    :: PlanId          -- ^ Unique string used to identify `Plan`
    -> Amount          -- ^ Positive integer in cents (or 0 for a free plan) representing how much to charge on a recurring basis
    -> Currency        -- ^ `Currency` of `Plan`
    -> Interval        -- ^ Billing Frequency 
    -> Name            -- ^ Name of `Plan` to be displayed on `Invoice`s
    -> TrialPeriodDays -- ^ Name of `Plan` to be displayed on `Invoice`s
    -> Stripe Plan
createPlanTrialPeriodDays
    planId
    amount
    currency
    interval
    name
    intervalCount = createPlanBase planId amount 
                    currency interval name
                    (Just intervalCount) Nothing Nothing



------------------------------------------------------------------------------
-- | Retrieve a `Plan`
getPlan
    :: PlanId -- ^ The ID of the plan to retrieve
    -> Stripe Plan
getPlan
    (PlanId planId) = callAPI request
  where request = StripeRequest GET url params
        url     = "plans" </> planId
        params  = []

------------------------------------------------------------------------------
-- | Base Request for updating a `Plan`, useful for creating customer `Plan` update functions
updatePlanBase
    :: PlanId            -- ^ The ID of the `Plan` to update
    -> Maybe Name        -- ^ The `Name` of the `Plan` to update
    -> Maybe Description -- ^ The `Description` of the `Plan` to update
    -> Stripe Plan
updatePlanBase
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
-- | Update a `Plan` `Description`
updatePlanDescription
    :: PlanId      -- ^ The ID of the `Plan` to update
    -> Description -- ^ The `Description` of the `Plan` to update
    -> Stripe Plan
updatePlanDescription
    (PlanId planId)
    description = callAPI request 
  where request = StripeRequest POST url params
        url     = "plans" </> planId
        params  = getParams [
                    ("statement_description", Just description)
                  ]

------------------------------------------------------------------------------
-- | Update a `Plan` `Name`
updatePlanName
    :: PlanId      -- ^ The ID of the `Plan` to update
    -> Description -- ^ The `Name` of the `Plan` to update
    -> Stripe Plan
updatePlanName
    (PlanId planId)
    name = callAPI request 
  where request = StripeRequest POST url params
        url     = "plans" </> planId
        params  = getParams [
                    ("name", Just name)
                  ]

------------------------------------------------------------------------------
-- | Delete a `Plan`
deletePlan
    :: PlanId -- ^ The ID of the `Plan` to delete
    -> Stripe StripeDeleteResult
deletePlan 
    (PlanId planId) = callAPI request 
  where request = StripeRequest DELETE url params
        url     = "plans" </> planId
        params  = []
