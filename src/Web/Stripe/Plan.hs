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
import           Web.Stripe.Internal.StripeError

config :: StripeConfig
config = StripeConfig "sk_test_zvqdM2SSA6WwySqM6KJQrqpH" "2014-03-28"

newtype PlanId          = PlanId Text deriving (Show, Eq)
newtype Name            = Name Text deriving (Show, Eq)
newtype Currency        = Currency Text deriving (Show, Eq)
newtype IntervalCount   = IntervalCount Int deriving (Show, Eq)
newtype TrialPeriodDays = TrialPeriodDays Int deriving (Show, Eq)
newtype Description     = Description Text deriving (Show, Eq)
newtype Amount          = Amount Int deriving (Show, Eq)
data Interval = Week | Month | Year deriving (Eq)

instance Show Interval where
    show Week = "week"
    show Month = "month"
    show Year = "year"

data Plan = Plan {
      planId              :: PlanId
    , planAmount          :: Int
    , planInterval        :: Interval
    , planCreated         :: UTCTime
    , planCurrency        :: Text
    , planLiveMode        :: Bool
    , planName            :: Text
    , planIntervalCount   :: Maybe Int -- optional, max of 1 year intervals allowed, default 1
    , planTrialPeriodDays :: Maybe Int
    , planMetaData        :: Maybe Object
    , planDescription     :: Maybe Text
} deriving (Show, Eq)

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

type Plans = StripeList Plan

-- works
getPlans :: Stripe Plans
getPlans = callAPI request 
  where request = StripeRequest GET "plans" params
        params  = []

instance FromJSON Plan where
   parseJSON (Object o) =
       do planId <- PlanId <$> o .: "id"
          planAmount <- o .: "amount"
          result <- o .: "interval"
          let planInterval = 
                  case String result of
                    "month" -> Month
                    "week" -> Week
                    "year" -> Year
          planCreated <- fromSeconds <$> o .: "created"
          planCurrency <- o .: "currency"
          planLiveMode <- o .: "livemode"
          planName <- o .: "name"
          planIntervalCount <- o .:? "interval_count"
          planTrialPeriodDays <- o .:? "trial_period_days"
          planMetaData <- o .:? "meta_data"
          planDescription <- o .:? "statement_description"
          return Plan {..}
