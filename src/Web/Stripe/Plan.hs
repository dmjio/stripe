{-# LANGUAGE OverloadedStrings #-}
module Web.Stripe.Plan 
    ( -- * API 
      createPlan
    , createPlanBase
    , getPlan
    , getPlans
    , updatePlanName
    , updatePlanDescription
    , updatePlanBase
    , deletePlan
      -- * Types
    , PlanId   (..)
    , Plan     (..)
    , Interval (..)
    ) where

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
    -> MetaData              -- ^ MetaData for the Plan
    -> Stripe Plan
createPlanBase
    (PlanId planId)
    amount
    (Currency currency)
    interval
    name
    intervalCount
    trialPeriodDays
    description 
    metadata    = callAPI request
  where request = StripeRequest POST url params
        url     = "plans"
        params  = toMetaData metadata ++ getParams [
                     ("id", Just planId) 
                   , ("amount", toText `fmap` Just amount) 
                   , ("currency", Just currency)
                   , ("interval", toText `fmap` Just interval) 
                   , ("name", Just name) 
                   , ("interval_count", (\(IntervalCount x) -> toText x) `fmap` intervalCount )
                   , ("trial_period_days", (\(TrialPeriodDays x) -> toText x) `fmap` trialPeriodDays )
                   , ("statement_description", description )
                 ]

------------------------------------------------------------------------------
-- | Create a `Plan`
createPlan 
    :: PlanId        -- ^ Unique string used to identify `Plan`
    -> Amount        -- ^ Positive integer in cents (or 0 for a free plan) representing how much to charge on a recurring basis
    -> Currency      -- ^ `Currency` of `Plan`
    -> Interval      -- ^ Billing Frequency 
    -> Name          -- ^ Name of `Plan` to be displayed on `Invoice`s
    -> MetaData      -- ^ MetaData for the Plan
    -> Stripe Plan
createPlan 
    planId
    amount
    currency
    intervalCount
    name = createPlanBase planId amount currency intervalCount name Nothing Nothing Nothing 

------------------------------------------------------------------------------
-- | Create a `Plan` with a specified `IntervalCount`
createPlanIntervalCount
    :: PlanId        -- ^ Unique string used to identify `Plan`
    -> Amount        -- ^ Positive integer in cents (or 0 for a free plan) representing how much to charge on a recurring basis
    -> Currency      -- ^ `Currency` of `Plan`
    -> Interval      -- ^ Billing Frequency 
    -> Name          -- ^ Name of `Plan` to be displayed on `Invoice`s
    -> IntervalCount -- ^ # of billins between each `Subscription` billing
    -> Stripe Plan
createPlanIntervalCount
    planId
    amount
    currency
    interval
    name
    intervalCount = createPlanBase planId amount 
                    currency interval name
                    (Just intervalCount) Nothing Nothing []

------------------------------------------------------------------------------
-- | Create a `Plan` with a specified number of `TrialPeriodDays`
createPlanTrialPeriodDays
    :: PlanId          -- ^ Unique string used to identify `Plan`
    -> Amount          -- ^ Positive integer in cents (or 0 for a free plan) representing how much to charge on a recurring basis
    -> Currency        -- ^ `Currency` of `Plan`
    -> Interval        -- ^ Billing Frequency 
    -> Name            -- ^ Name of `Plan` to be displayed on `Invoice`s
    -> TrialPeriodDays -- ^ Number of Trial Period Days to be displayed on `Invoice`s
    -> Stripe Plan
createPlanTrialPeriodDays
    planId
    amount
    currency
    interval
    name
    trialPeriodDays = 
        createPlanBase
          planId
          amount
          currency
          interval
          name
          Nothing
          (Just trialPeriodDays) 
          Nothing 
          []

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
-- | Retrieve a `Plan`
getPlans
    :: Limit                -- ^ Defaults to 10 if `Nothing` specified
    -> StartingAfter PlanId -- ^ Paginate starting after the following `CustomerID`
    -> EndingBefore PlanId  -- ^ Paginate ending before the following `CustomerID`
    -> Stripe Plan
getPlans
    limit
    startingAfter
    endingBefore = callAPI request
  where request = StripeRequest GET url params
        url     = "plans"
        params  = getParams [
            ("limit", toText `fmap` limit )
          , ("starting_after", (\(PlanId x) -> x) `fmap` startingAfter)
          , ("ending_before", (\(PlanId x) -> x) `fmap` endingBefore)
          ]

------------------------------------------------------------------------------
-- | Base Request for updating a `Plan`, useful for creating customer `Plan` update functions
updatePlanBase
    :: PlanId            -- ^ The ID of the `Plan` to update
    -> Maybe Name        -- ^ The `Name` of the `Plan` to update
    -> Maybe Description -- ^ The `Description` of the `Plan` to update
    -> MetaData          -- ^ The `MetaData` for the `Plan`
    -> Stripe Plan
updatePlanBase
    (PlanId planId)
    name
    description 
    metadata    = callAPI request 
  where request = StripeRequest POST url params
        url     = "plans" </> planId
        params  = toMetaData metadata ++ getParams [
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
