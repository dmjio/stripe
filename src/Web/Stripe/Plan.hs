{-# LANGUAGE OverloadedStrings #-}

module Web.Stripe.Plan 
    ( -- * Plan Types
      PlanId   (..)
    , Plan     (..)
    , Interval (..)
      -- * API Functions
    , createPlan
    , getPlan
    , updatePlan
    , deletePlan
    ) where

import           Control.Applicative             ((<$>), (<*>))
import           Data.Text                       (Text)
import qualified Data.Text.Encoding as T

import           Web.Stripe.Client.Internal
import           Web.Stripe.Types

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
    (Name name) 
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
                   , ("statement_description", (\(Description x) -> x) <$> description )
                 ]

getPlan
    :: PlanId -- ^ The ID of the plan to retrieve
    -> Stripe Plan
getPlan
    (PlanId planId) = callAPI request
  where request = StripeRequest GET url params
        url     = "plans" </> planId
        params  = []

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
                      ("name", (\(Name x) -> x) <$> name )
                    , ("statement_description", (\(Description x) -> x) <$> description )
                  ]

deletePlan
    :: PlanId -- ^ The ID of the plan to delete
    -> Stripe StripeDeleteResult
deletePlan 
    (PlanId planId) = callAPI request 
  where request = StripeRequest DELETE url params
        url     = "plans" </> planId
        params  = []
