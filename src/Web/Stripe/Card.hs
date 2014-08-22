{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}

module Web.Stripe.Card where

import           Control.Applicative
import           Data.Aeson
import           Data.ByteString                 (ByteString)
import qualified Data.ByteString                 as B
import           Data.Monoid
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import qualified Data.Text.Encoding              as T
import           Data.Time
import           Network.Http.Client
import           Web.Stripe.Client
import           Web.Stripe.Internal.StripeError
import           Web.Stripe.Util

config :: StripeConfig
config = StripeConfig "sk_test_zvqdM2SSA6WwySqM6KJQrqpH" "2014-03-28"

data Brand = Visa
           | AMEX
           | MasterCard
           | Discover
           | JCB
           | DinersClub
           | Unknown
             deriving (Show, Eq)

data Card = Card {
      cardId                  :: Text
    , cardLastFour            :: Text
    , cardBrand               :: Brand
    , cardFunding             :: Text
    , cardExpMonth            :: Int
    , cardExpYear             :: Int
    , cardFingerprint         :: Text
    , cardCountry             :: Text
    , cardName                :: Maybe Text
    , cardAddress_line1       :: Maybe Text
    , cardAddress_line2       :: Maybe Text
    , cardAddress_city        :: Maybe Text
    , cardAddress_state       :: Maybe Text
    , cardAddress_zip         :: Maybe Text
    , cardAddress_country     :: Maybe Text
    , cardCvc_check           :: Maybe Text
    , cardAddress_line1_check :: Maybe Text
    , cardAddress_zip_check   :: Maybe Text
    , cardCustomer            :: Maybe Text
} deriving (Show, Eq)

newtype CardId = CardId Text deriving (Show, Eq)
newtype TokenId = TokenId Text deriving (Show, Eq, Ord)
newtype CustomerId = CustomerId Text deriving (Show, Eq, Ord)
newtype CardNumber = CardNumber Int deriving (Show, Eq, Ord)
newtype ExpMonth   = ExpMonth Int deriving (Show, Eq, Ord)
newtype ExpYear    = ExpYear Int deriving (Show, Eq, Ord)
newtype CVC        = CVC Int deriving (Show, Eq, Ord)
newtype AddressCity    = AddressCity Text deriving (Show, Eq)
newtype AddressCountry = AddressCountry Text deriving (Show, Eq)
newtype AddressLine1   = AddressLine1 Text deriving (Show, Eq)
newtype AddressLine2   = AddressLine2 Text deriving (Show, Eq)
newtype AddressState   = AddressState Text deriving (Show, Eq)
newtype AddressZip     = AddressZip Text deriving (Show, Eq)
newtype Name           = Name Text deriving (Show, Eq)
newtype EndingBefore = EndingBefore Text deriving (Show, Eq)
newtype StartingAfter = StartingAfter Text deriving (Show, Eq)
newtype Limit = Limit Int deriving (Show, Eq)

data StripeResult = StripeResult { deleted :: Bool, deletedId :: Text } deriving (Show, Eq)
data StripeList a = StripeList { hasMore :: Bool, stripeList :: [a] } deriving (Show, Eq)

data Token = Token {
      tokenId       :: Text
    , tokenLiveMode :: Bool
    , tokenCreated  :: UTCTime
    , tokenUsed     :: Bool
    , tokenType     :: Text
    , tokenCard     :: Card
} deriving (Show, Eq)

instance FromJSON Brand where
    parseJSON (String result) =
        return $ case result of
                   "American Express" -> AMEX
                   "MasterCard" -> MasterCard
                   "Discover" -> Discover
                   "JCB" -> JCB
                   "Visa" -> Visa
                   "DinersClub" -> DinersClub
                   otherwise -> Unknown

instance FromJSON Card where
    parseJSON (Object o) =
        Card <$> o .: "id"
             <*> o .: "last4"
             <*> o .: "brand"
             <*> o .: "funding"
             <*> o .: "exp_month"
             <*> o .: "exp_year"
             <*> o .: "fingerprint"
             <*> o .: "country"
             <*> o .:? "name"
             <*> o .:? "address_line1"
             <*> o .:? "address_line2"
             <*> o .:? "address_city"
             <*> o .:? "address_state"
             <*> o .:? "address_zip"
             <*> o .:? "address_country"
             <*> o .:? "cvc_check"
             <*> o .:? "address_line1_check"
             <*> o .:? "address_zip_check"
             <*> o .:? "customer"

instance FromJSON StripeResult where
   parseJSON (Object o) =
       StripeResult <$> o .: "deleted"
                    <*> o .: "id"

instance FromJSON Token where
   parseJSON (Object o) =
       Token <$> o .: "id"
             <*> o .: "livemode"
             <*> (fromSeconds <$> o .: "created")
             <*> o .: "used"
             <*> o .: "type"
             <*> o .: "card"

instance FromJSON a => FromJSON (StripeList a) where
   parseJSON (Object o) =
       StripeList <$> o .: "has_more"
                  <*> o .: "data"

-- | Create card
addCardToCustomer :: FromJSON a => CustomerId -> TokenId -> IO (Either StripeError a)
addCardToCustomer (CustomerId cid) (TokenId tokenId) =
    sendStripeRequest config req params
  where req = StripeRequest POST url
        url = "customers/" <> cid <> "/cards"
        params = [("card", toBS tokenId)]

-- | Create a Stripe token from a credit card
createCardToken :: CardNumber ->
                   ExpMonth ->
                   ExpYear ->
                   CVC ->
                   IO (Either StripeError Token)
createCardToken (CardNumber num) (ExpMonth month) (ExpYear year) (CVC cvc)
    = sendStripeRequest config req params
  where req = StripeRequest POST "tokens"
        params = [ ("card[number]", toBS num)
                 , ("card[exp_month]", toBS month)
                 , ("card[exp_year]", toBS year)
                 , ("card[cvc]", toBS cvc)
                 ]

-- | Get card by CustomerID and CardID
getCard :: CustomerId -> CardId -> IO (Either StripeError Card)
getCard (CustomerId custId) (CardId cardId) = sendStripeRequest config req []
  where req = StripeRequest GET url
        url = "customers/" <> custId <> "/cards/" <> cardId


updateCard :: CustomerId ->
              CardId ->
              Maybe AddressCity -> -- addressCity
              Maybe AddressCountry -> -- addressCountry
              Maybe AddressLine1 -> -- addressLine1
              Maybe AddressLine2 -> -- addressLine2
              Maybe AddressState -> -- addressState
              Maybe AddressZip -> -- addressZip
              Maybe ExpMonth -> -- expMonth
              Maybe ExpYear -> -- expYear
              Maybe Name -> -- name
              IO (Either StripeError Card)
updateCard (CustomerId custId)
           (CardId cardId)
           addressCity
           addressCountry
           addressLine1
           addressLine2
           addressState
           addressZip
           expMonth
           expYear
           name = sendStripeRequest config req params
  where req = StripeRequest POST url
        url = "customers/" <> custId <> "/cards/" <> cardId
        params = [ (k, v) | (k, Just v) <- [
                     ("address_city",  (\(AddressCity x) -> toBS x) <$> addressCity)
                   , ("address_country", (\(AddressCountry x) -> toBS x) <$> addressCountry)
                   , ("address_line1", (\(AddressLine1 x) -> toBS x) <$> addressLine1 )
                   , ("address_line2", (\(AddressLine2 x) -> toBS x) <$> addressLine2 )
                   , ("address_state", (\(AddressState x) -> toBS x) <$> addressState )
                   , ("address_zip", (\(AddressZip x) -> toBS x) <$> addressZip )
                   , ("exp_month", (\(ExpMonth x) -> toBS x) <$> expMonth )
                   , ("exp_year", (\(ExpYear x) -> toBS x) <$> expYear )
                   , ("name", (\(Name x) -> toBS x) <$> name )
                   ]
                 ]

deleteCard :: CustomerId -> CardId -> IO (Either StripeError StripeResult)
deleteCard (CustomerId custId) (CardId cardId) = sendStripeRequest config req []
  where req = StripeRequest DELETE url
        url = "customers/" <> custId <> "/cards/" <> cardId

getCards :: CustomerId ->
            Maybe Limit -> -- Default is 10
            Maybe EndingBefore -> -- For use in pagination
            Maybe StartingAfter -> -- For use in pagination
            IO (Either StripeError (StripeList Card))
getCards (CustomerId custId) limit endingBefore startingAfter =
    sendStripeRequest config req params
  where req = StripeRequest GET url
        url = "customers/" <> custId <> "/cards"
        params = [ (k, v) | (k, Just v) <- [
                     ("ending_before",  (\(EndingBefore x) -> toBS x) <$> endingBefore)
                   , ("starting_after",  (\(StartingAfter x) -> toBS x) <$> startingAfter)
                   , ("limit", (\(Limit x) -> toBS x) <$> limit)
                   ]
                 ]

--------------- Subscriptions ----------------
newtype SubscriptionId = SubscriptionId Text deriving (Show, Eq)
newtype PlanId = PlanId Text deriving (Show, Eq)

data Subscription = Subscription {
      subscriptionId :: Text
} deriving (Show, Eq)

instance FromJSON Subscription where
   parseJSON (Object o) = Subscription <$> o .: "id"

createSubscription :: CustomerId -> PlanId -> IO (Either StripeError Subscription)
createSubscription (CustomerId custId) (PlanId plan) =
    sendStripeRequest config req params
  where req = StripeRequest POST url
        url = "customers/" <> custId <> "/subscriptions"
        params = [("plan", T.encodeUtf8 plan)]

getSubscription :: CustomerId -> SubscriptionId -> IO (Either StripeError Subscription)
getSubscription (CustomerId custId) (SubscriptionId subId) =
    sendStripeRequest config req []
  where req = StripeRequest GET url
        url = "customers/" <> custId <> "/subscriptions/" <> subId

-- see parameters on this one
updateSubscription :: CustomerId -> SubscriptionId -> IO (Either StripeError Subscription)
updateSubscription (CustomerId custId) (SubscriptionId subId) =
    sendStripeRequest config req []
  where req = StripeRequest POST url
        url = "customers/" <> custId <> "/subscriptions/" <> subId

deleteSubscription :: CustomerId -> SubscriptionId -> IO (Either StripeError Subscription)
deleteSubscription (CustomerId custId) (SubscriptionId subId) =
    sendStripeRequest config req []
  where req = StripeRequest DELETE url
        url = "customers/" <> custId <> "/subscriptions/" <> subId

--- ===== plans ====== --

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

--------------- Coupons ------------------

-- -- see optional
-- -- You must set either percent_off or amount_off and currency

data Duration = Forever | Once | Repeating deriving Eq

instance Show Duration where
    show Forever = "forever"
    show Once = "once"
    show Repeating = "repeating"

instance FromJSON Duration where
   parseJSON (String x) 
       | x == "forever" = pure Forever
       | x == "once" = pure Once
       | x == "repeating" = pure Repeating
       | otherwise = error "Invalid Duration"

data Coupon = Coupon {
      couponId :: Text
    , couponCreated :: UTCTime
    , couponPercentOff :: Int
    , couponAmountOff :: Maybe Int
    , couponCurrency :: Maybe Text
    , couponLiveMode :: Bool
    , couponDuration :: Duration
    , couponRedeemBy :: Maybe UTCTime
    , couponMaxRedemptions :: Maybe Int
    , couponTimesRedeemed :: Maybe Int
    , couponDurationInMonths :: Maybe Int
    , couponValid :: Bool
    } deriving (Show, Eq)

instance FromJSON Coupon where
   parseJSON (Object o) = 
        Coupon <$> o .: "id"
               <*> (fromSeconds <$> o .: "created")
               <*> o .: "percent_off"
               <*> o .:? "amount_off"
               <*> o .:? "currency"
               <*> o .: "livemode"
               <*> o .: "duration"
               <*> (fmap fromSeconds <$> o .:? "redeem_by")
               <*> o .:? "max_redemptions"
               <*> o .:? "times_redeemed"
               <*> o .:? "duration_in_months"
               <*> o .: "valid"

newtype CouponId = CouponId Text deriving (Show, Eq)
newtype AmountOff = AmountOff Int deriving (Show, Eq)
newtype MaxRedemptions = MaxRedemptions Int deriving (Show, Eq)
newtype PercentOff = PercentOff Int deriving (Show, Eq)
newtype RedeemBy = RedeemBy UTCTime deriving (Show, Eq)
newtype DurationInMonths = DurationInMonths Int deriving (Show, Eq)

createCoupon :: Maybe CouponId -> 
                Duration ->
                Maybe AmountOff ->
                Maybe Currency ->
                Maybe DurationInMonths ->
                Maybe MaxRedemptions ->
                Maybe PercentOff ->
                Maybe RedeemBy ->
                IO (Either StripeError Coupon)                 
createCoupon couponId duration amountOff currency durationInMonths maxRedemptions percentOff redeemBy
    = sendStripeRequest config req params
  where req = StripeRequest POST url 
        url = "coupons"
        params = [(k,v) | (k, Just v) <- [ 
                    ("id", (\(CouponId x) -> toBS x) <$> couponId )
                  , ("duration", Just $ toBS duration )
                  , ("amount_off", (\(AmountOff x) -> toBS x) <$> amountOff )
                  , ("currency", (\(Currency x) -> T.encodeUtf8 x) <$> currency )
                  , ("duration_in_months", (\(DurationInMonths x) -> toBS x) <$> durationInMonths )
                  , ("max_redemptions", (\(MaxRedemptions x) -> toBS x) <$> maxRedemptions )
                  , ("percent_off", (\(PercentOff x) -> toBS x) <$> percentOff )
                  , ("redeem_by", (\(RedeemBy x) -> toBS x) <$> redeemBy )
                  ]
                 ]
-- works
getCoupon :: CouponId -> IO (Either StripeError Coupon)
getCoupon (CouponId couponId) = sendStripeRequest config req []
  where req = StripeRequest POST url 
        url = "coupons/" <> couponId

-- delete coupon
deleteCoupon :: CouponId -> IO (Either StripeError StripeResult)
deleteCoupon (CouponId couponId) = sendStripeRequest config req []
  where req = StripeRequest DELETE $ "coupons/" <> couponId

type Coupons = StripeList Coupon

-- works but needs more options, like ending and starting
getCoupons :: IO (Either StripeError Coupons)
getCoupons = sendStripeRequest config req []
  where req = StripeRequest GET "coupons"


-- refunds
-- -- create a refund
data Refund = Refund {
      refundId :: Text
    , refundAmount :: Int
    , refundCurrency :: Text
    , refundCreated :: UTCTime
    , refundCharge :: Text
    , refundBalanceTransaction :: Maybe Text
    } deriving (Show, Eq)

instance FromJSON Refund where
   parseJSON (Object o) = 
        Refund <$> o .: "id"
               <*> o .: "amount"
               <*> o .: "currency"
               <*> (fromSeconds <$> o .: "created")
               <*> o .: "charge"
               <*> o .:? "balance_transaction"

newtype ChargeId = ChargeId Text deriving (Show, Eq)
newtype RefundId = RefundId Text deriving (Show, Eq)

-- works, see optional parameters
createRefund :: ChargeId -> IO (Either StripeError Refund)
createRefund (ChargeId chargeId) = sendStripeRequest config req []
  where req = StripeRequest POST url 
        url = "charges/" <> chargeId <> "/refunds"

getRefund :: ChargeId -> RefundId -> IO (Either StripeError Refund)
getRefund (ChargeId chargeId) (RefundId refId) = sendStripeRequest config req []
   where req = StripeRequest GET $ "charges/" <> chargeId <> "/refunds/" <> refId

-- -- optional metadata
-- updateRefund :: Charge -> RefundId -> IO ()
-- updateRefund (Charge chargeId) (RefundId refId) = sendStripeRequest req config
--   where req = StripeRequest POST url []
--         url = "charges/" <> chargeId <> "/refunds/" <> refId

-- -- optional, limit, etc
type Refunds = StripeList Refund

-- works but needs more parameters
getRefunds :: ChargeId -> IO (Either StripeError Refunds)
getRefunds (ChargeId chargeId) = sendStripeRequest config req []
  where req = StripeRequest GET $ "charges/" <> chargeId <> "/refunds"


-- -- Discounts
-- deleteDiscount :: FromJSON a => CustomerId -> IO (Either StripeError a)
-- deleteDiscount (CustomerId customerId) = sendStripeRequest config req []
--   where req = StripeRequest DELETE url 
--         url = "customers/" <> customerId <> "/discount"

-- deleteSubscriptionDiscount :: FromJSON a =>
--   CustomerId -> SubscriptionId -> IO (Either StripeError a)
-- deleteSubscriptionDiscount (CustomerId customerId) (SubscriptionId subId) =
--     sendStripeRequest config req []
--   where req = StripeRequest DELETE url 
--         url = T.concat ["customers/"
--                        , customerId
--                        , "/subscriptions/"
--                        , subId
--                        , "/discount"
--                        ]
