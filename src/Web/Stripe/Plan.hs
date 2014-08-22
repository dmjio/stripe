module Web.Stripe.Plan where

import           Control.Applicative             ((<$>), (<*>))
import           Data.Aeson
import           Data.Monoid
import           Data.Text                       (Text)
import           Data.Time
import           Web.Stripe.Client.Internal
import           Web.Stripe.Util
import           Web.Stripe.Internal.StripeError


data Interval = Week | Month | Year 
                deriving (Eq)

instance Show Interval where
    show Week = "week"
    show Month = "month"
    show Year = "year"

-- Plans
data Plan = Plan {
      planId              :: Text
    , planAmount          :: Int
    , planInterval        :: Interval
    , planCreated         :: UTCTime
    , planCurrency        :: Text
    , planLiveMode        :: Bool
    , planName            :: Text
    , planIntervalCount   :: Maybe Int -- optional, max of 1 year intervals allowed, default 1
    , planTrialPeriodDays :: Maybe Int
    , planMetaData        :: Maybe Object
    , planDescription      :: Maybe Text
} deriving (Show, Eq)


instance FromJSON Plan where
   parseJSON (Object o) =
       do planId <- o .: "id"
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

-- check all the optional ones as well
newtype Currency = Currency Text deriving (Show, Eq)
newtype IntervalCount = IntervalCount Int deriving (Show, Eq)
newtype TrialPeriodDays = TrialPeriodDays Int deriving (Show, Eq)
newtype Description = Description Text deriving (Show, Eq)
newtype Amount = Amount Int deriving (Show, Eq)

createPlan :: PlanId -> 
              Amount -> 
              Currency ->
              Interval ->
              Name ->
              Maybe IntervalCount ->
              Maybe TrialPeriodDays ->
              Maybe Description ->
              IO (Either StripeError Plan)
createPlan (PlanId planId) (Amount amount) (Currency currency) interval (Name name) 
           intervalCount trialPeriodDays description
    = sendStripeRequest config req params
  where req = StripeRequest POST url
        url = "plans"
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

-- gets plan * works
getPlan :: PlanId -> IO (Either StripeError Plan)
getPlan (PlanId planId) = sendStripeRequest config req []
  where req = StripeRequest GET url 
        url = "plans/" <> planId

-- optional :: name, metadata, statement_description * works
updatePlan :: PlanId ->
              Maybe Name -> 
              Maybe Description ->
              IO (Either StripeError Plan)
updatePlan (PlanId planId) name description = sendStripeRequest config req params
  where req = StripeRequest POST $ "plans/" <> planId
        params = [ (k,v) | (k, Just v) <- [
                     ("name", (\(Name x) -> T.encodeUtf8 x) <$> name )
                   , ("statement_description", (\(Description x) -> T.encodeUtf8 x) <$> description )
                   ]
                 ]
-- works
deletePlan :: PlanId -> IO (Either StripeError StripeResult)                 
deletePlan (PlanId planId) = sendStripeRequest config req []
  where req = StripeRequest DELETE $ "plans/" <> planId

type Plans = StripeList Plan

-- works
getPlans :: IO (Either StripeError Plans)
getPlans = sendStripeRequest config req []
  where req = StripeRequest GET "plans"
