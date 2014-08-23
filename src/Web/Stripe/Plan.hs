{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Web.Stripe.Plan 
    ( createPlan
    , getPlan
    , updatePlan
    , deletePlan
    , getPlans
    , PlanId (..)
    , Plan (..)
    , Interval (..)
    )
    where

import           Control.Applicative             ((<$>), (<*>))
import           Data.Aeson
import           Data.Monoid
import           Data.Text                       (Text)
import qualified Data.Text.Encoding as T
import           Data.Time
import           Web.Stripe.Client.Internal
import           Web.Stripe.Util

config :: StripeConfig
config = StripeConfig "sk_test_zvqdM2SSA6WwySqM6KJQrqpH" "2014-03-28"

createPlan :: PlanId -> 
              Amount -> 
              Currency ->
              Interval ->
              Name ->
              Maybe IntervalCount ->
              Maybe TrialPeriodDays ->
              Maybe Description ->
              Stripe Plan
createPlan (PlanId planId) (Amount amount) (Currency currency) interval (Name name) 
           intervalCount trialPeriodDays description = callAPI request
  where request = StripeRequest POST url params
        url     = "plans"
        params = [ (k,v) | (k, Just v) <- [ 
                     ("id", Just $ T.encodeUtf8 planId) -- required
                   , ("amount", Just $ toBS amount) -- required
                   , ("currency", Just $ T.encodeUtf8 currency) -- required
                   , ("interval", Just $ toBS interval) -- required
                   , ("name", Just $ T.encodeUtf8 name) -- required
                   , ("interval_count", (\(IntervalCount x) -> toBS x) <$> intervalCount )
                   , ("trial_period_days", (\(TrialPeriodDays x) -> toBS x) <$> trialPeriodDays )
                   , ("statement_description", (\(Description x) -> T.encodeUtf8 x) <$> description )
                   ]
                 ]

getPlan :: PlanId -> Stripe Plan
getPlan (PlanId planId) = callAPI request
  where request = StripeRequest GET url params
        url     = "plans/" <> planId
        params  = []

-- optional :: name, metadata, statement_description * works
updatePlan :: PlanId ->
              Maybe Name -> 
              Maybe Description ->
              Stripe Plan
updatePlan (PlanId planId) name description = callAPI request 
  where request = StripeRequest POST url params
        url     = "plans/" <> planId
        params  = [ (k,v) | (k, Just v) <- [
                      ("name", (\(Name x) -> T.encodeUtf8 x) <$> name )
                    , ("statement_description", (\(Description x) -> T.encodeUtf8 x) <$> description )
                    ]
                  ]

deletePlan :: PlanId -> Stripe StripeDeleteResult
deletePlan (PlanId planId) = callAPI request 
  where request = StripeRequest DELETE url params
        url     = "plans/" <> planId
        params  = []
