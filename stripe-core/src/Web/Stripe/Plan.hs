{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
-------------------------------------------
-- |
-- Module      : Web.Stripe.Plan
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- < https:/\/\stripe.com/docs/api#plans >
--
-- @
-- import Web.Stripe
-- import Web.Stripe.Plan
--
-- main :: IO ()
-- main = do
--   let config = SecretKey "secret_key"
--   result <- stripe config $ do
--       createPlan (PlanId "free plan")
--                  (0 :: Amount)
--                  (USD :: Currency)
--                  (Month :: Interval)
--                  ("a sample free plan" :: Name)
--                  ([] :: MetaData)
--   case result of
--     Right plan     -> print plan
--     Left stripeError -> print stripeError
-- @
module Web.Stripe.Plan
    ( -- * API
      CreatePlan
    , createPlan
    , GetPlan
    , getPlan
    , UpdatePlan
    , updatePlan
    , DeletePlan
    , deletePlan
    , GetPlans
    , getPlans
      -- * Types
    , PlanId              (..)
    , Plan                (..)
    , Interval            (..)
    , StripeList          (..)
    , IntervalCount       (..)
    , TrialPeriodDays     (..)
    , StripeDeleteResult  (..)
    , Currency            (..)
    , Limit               (..)
    , StartingAfter       (..)
    , EndingBefore        (..)
    , PlanName            (..)
    , Amount              (..)
    , StatementDescription(..)
    , MetaData            (..)
    ) where

import           Data.Text                (Text)
import           Web.Stripe.StripeRequest (Method (GET, POST, DELETE), Param(..),
                                           StripeHasParam, StripeRequest (..),
                                           StripeReturn, ToStripeParam(..),
                                           mkStripeRequest)
import           Web.Stripe.Types         (PlanId (..) , Plan (..), PlanName(..), Interval (..), StripeList(..), IntervalCount (..), TrialPeriodDays (..), Limit, StartingAfter, EndingBefore, StripeDeleteResult(..), Currency (..), Name, Amount, StatementDescription, MetaData)
import           Web.Stripe.Util    ( (</>))

------------------------------------------------------------------------------
-- | Create a `Plan`
data CreatePlan
type instance StripeReturn CreatePlan = Plan
instance StripeHasParam CreatePlan IntervalCount
instance StripeHasParam CreatePlan TrialPeriodDays
instance StripeHasParam CreatePlan MetaData
instance StripeHasParam CreatePlan StatementDescription
createPlan
    :: PlanId                -- ^ Unique string used to identify `Plan`
    -> Amount                -- ^ Positive integer in cents representing how much to charge on a recurring basis
    -> Currency              -- ^ `Currency` of `Plan`
    -> Interval              -- ^ Billing Frequency (i.e. `Day`, `Week` or `Month`)
    -> PlanName              -- ^ Name of `Plan` to be displayed on `Invoice`s
    -> StripeRequest CreatePlan
createPlan
  (PlanId planid)
  amount
  currency
  interval
  name = request
  where request = mkStripeRequest POST url params
        url     = "plans"
        params  = toStripeParam (Param ("id" :: Text, planid)) $
                  toStripeParam amount $
                  toStripeParam currency $
                  toStripeParam interval $
                  toStripeParam name $
                  []

------------------------------------------------------------------------------
-- | Retrieve a `Plan`
data GetPlan
type instance StripeReturn GetPlan = Plan
getPlan
    :: PlanId -- ^ The ID of the plan to retrieve
    -> StripeRequest GetPlan
getPlan
    (PlanId planid) = request
  where request = mkStripeRequest GET url params
        url     = "plans" </> planid
        params  = []

------------------------------------------------------------------------------
-- | Update a `Plan`
data UpdatePlan
type instance StripeReturn UpdatePlan = Plan
instance StripeHasParam UpdatePlan PlanName
instance StripeHasParam UpdatePlan MetaData
instance StripeHasParam UpdatePlan StatementDescription
updatePlan
    :: PlanId            -- ^ The ID of the `Plan` to update
    -> StripeRequest UpdatePlan
updatePlan
    (PlanId planid)
                = request
  where request = mkStripeRequest POST url params
        url     = "plans" </> planid
        params  = []

------------------------------------------------------------------------------
-- | Delete a `Plan`
data DeletePlan
type instance StripeReturn DeletePlan = StripeDeleteResult
deletePlan
    :: PlanId -- ^ The ID of the `Plan` to delete
    -> StripeRequest DeletePlan
deletePlan
    (PlanId planid) = request
  where request = mkStripeRequest DELETE url params
        url     = "plans" </> planid
        params  = []

------------------------------------------------------------------------------
-- | Retrieve all  `Plan`s
data GetPlans
type instance StripeReturn GetPlans = (StripeList Plan)
instance StripeHasParam GetPlans (EndingBefore PlanId)
instance StripeHasParam GetPlans Limit
instance StripeHasParam GetPlans (StartingAfter PlanId)

getPlans :: StripeRequest GetPlans
getPlans = request
  where request = mkStripeRequest GET url params
        url     = "plans"
        params  = []
