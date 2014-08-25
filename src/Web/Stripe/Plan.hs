{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Web.Stripe.Plan 
    ( -- * Types
      PlanId (..)
    , Plan (..)
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
import           Web.Stripe.Util
import           Web.Stripe.Types

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
                     ("id", Just $ T.encodeUtf8 planId) 
                   , ("amount", Just $ toBS amount) 
                   , ("currency", Just $ T.encodeUtf8 currency)
                   , ("interval", Just $ toBS interval) 
                   , ("name", Just $ T.encodeUtf8 name) 
                   , ("interval_count", (\(IntervalCount x) -> toBS x) <$> intervalCount )
                   , ("trial_period_days", (\(TrialPeriodDays x) -> toBS x) <$> trialPeriodDays )
                   , ("statement_description", (\(Description x) -> T.encodeUtf8 x) <$> description )
                   ]
                 ]

getPlan :: PlanId -> -- ^ The ID of the plan to retrieve
           Stripe Plan
getPlan (PlanId planId) = callAPI request
  where request = StripeRequest GET url params
        url     = "plans" </> planId
        params  = []

updatePlan :: PlanId ->            -- ^ The ID of the plan to update
              Maybe Name ->        -- ^ The name of the Plan to update
              Maybe Description -> -- ^ The name of the Plan to update
              Stripe Plan
updatePlan (PlanId planId) name description = callAPI request 
  where request = StripeRequest POST url params
        url     = "plans" </> planId
        params  = [ (k,v) | (k, Just v) <- [
                      ("name", (\(Name x) -> T.encodeUtf8 x) <$> name )
                    , ("statement_description", (\(Description x) -> T.encodeUtf8 x) <$> description )
                    ]
                  ]

deletePlan :: PlanId -> -- ^ The ID of the plan to delete
              Stripe StripeDeleteResult
deletePlan (PlanId planId) = callAPI request 
  where request = StripeRequest DELETE url params
        url     = "plans" </> planId
        params  = []
