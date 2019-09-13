{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE GeneralizedNewtypeDeriving    #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
------------------------------------------------------------------------------
-- |
-- Module      : Web.Stripe.Types
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
------------------------------------------------------------------------------
module Web.Stripe.Types where
------------------------------------------------------------------------------
import           Control.Applicative (pure, (<$>), (<*>), (<|>))
import           Control.Monad       (mzero)
import           Data.Aeson          (FromJSON (parseJSON), ToJSON(..), withText, withObject,
                                      Value (String, Object, Bool), (.:),
                                      (.:?))
import           Data.Aeson.Types    (typeMismatch)
import           Data.Data           (Data, Typeable)
import qualified Data.HashMap.Strict as H
import           Data.Ratio          ((%))
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Data.Time           (UTCTime)
import           Data.Monoid         ((<>))
import           Numeric             (fromRat, showFFloat)
import           Text.Read           (lexP, pfail)
import qualified Text.Read           as R
import           Web.Stripe.Util     (fromSeconds)
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- | `Expandable` values
--    maps from an id to an object, e.g. `CardId` to `Card`
type family ExpandsTo id :: *

-- | a wrapper for fields which can either be an id or an expanded object
data Expandable id
  = Id id -- ^ an id such as `CardId`, `AccountId`, `CustomerId`, etc
  | Expanded (ExpandsTo id) -- ^ expanded object such as `Card`, `Account`, `Customer`, etc
    deriving (Typeable)

deriving instance (Data id, Data (ExpandsTo id)) => Data (Expandable id)
deriving instance (Show id, Show (ExpandsTo id)) => Show (Expandable id)
deriving instance (Read id, Read (ExpandsTo id)) => Read (Expandable id)
deriving instance (Eq   id, Eq   (ExpandsTo id)) => Eq   (Expandable id)
deriving instance (Ord  id, Ord  (ExpandsTo id)) => Ord  (Expandable id)

type instance ExpandsTo AccountId       = Account
type instance ExpandsTo ApplicationId   = Application
type instance ExpandsTo CardId          = Card
type instance ExpandsTo ChargeId        = Charge
type instance ExpandsTo CustomerId      = Customer
type instance ExpandsTo InvoiceId       = Invoice
type instance ExpandsTo InvoiceItemId   = InvoiceItem
type instance ExpandsTo PaymentMethodId = PaymentMethod
type instance ExpandsTo RecipientId     = Recipient
type instance ExpandsTo RecipientCardId = RecipientCard
type instance ExpandsTo TransactionId   = BalanceTransaction

------------------------------------------------------------------------------
-- | JSON Instance for `Expandable`
instance (FromJSON id,  FromJSON (ExpandsTo id)) =>
         FromJSON (Expandable id) where
  parseJSON v = (Id <$> parseJSON v) <|> (Expanded <$> parseJSON v)

------------------------------------------------------------------------------
-- | specify a `TimeRange`
-- FIXME: this is a little awkward to use. How can we make it moar better?
data TimeRange a = TimeRange
    { gt  :: Maybe a
    , gte :: Maybe a
    , lt  :: Maybe a
    , lte :: Maybe a
    } deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | Time range with all values set to `Nothing`
emptyTimeRange :: TimeRange a
emptyTimeRange = TimeRange
    { gt  = Nothing
    , gte = Nothing
    , lt  = Nothing
    , lte = Nothing
    }

------------------------------------------------------------------------------
-- | `AvailableOn`
newtype AvailableOn = AvailableOn UTCTime
    deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | `Created`
newtype Created = Created UTCTime
    deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | `Date`
newtype Date = Date UTCTime
    deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | `ChargeId` associated with a `Charge`
newtype ChargeId
  = ChargeId Text
  deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `ChargeId`
instance FromJSON ChargeId where
   parseJSON (String x)   = pure $ ChargeId x
   parseJSON _ = mzero

------------------------------------------------------------------------------
-- | `StatementDescription` to be added to a `Charge`
newtype StatementDescription =
  StatementDescription Text deriving (Read, Show, Eq, Ord, Data, Typeable)

instance FromJSON StatementDescription where
  parseJSON v = StatementDescription <$> parseJSON v

------------------------------------------------------------------------------
-- | `Charge` object in `Stripe` API
data Charge = Charge {
      chargeId                   :: ChargeId
    , chargeObject               :: Text
    , chargeCreated              :: UTCTime
    , chargeLiveMode             :: Bool
    , chargePaid                 :: Bool
    , chargeAmount               :: Amount
    , chargeCurrency             :: Currency
    , chargeRefunded             :: Bool
    , chargeCreditCard           :: Maybe Card
    , chargeCaptured             :: Bool
    , chargeRefunds              :: StripeList Refund
    , chargeBalanceTransaction   :: Maybe (Expandable TransactionId)
    , chargeFailureMessage       :: Maybe Text
    , chargeFailureCode          :: Maybe Text
    , chargeAmountRefunded       :: Int
    , chargeCustomerId           :: Maybe (Expandable CustomerId)
    , chargeInvoice              :: Maybe (Expandable InvoiceId)
    , chargeDescription          :: Maybe Description
    , chargeDispute              :: Maybe Dispute
    , chargeMetaData             :: MetaData
    , chargeStatementDescription :: Maybe StatementDescription
    , chargeReceiptEmail         :: Maybe Text
    , chargeReceiptNumber        :: Maybe Text
    } deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `Charge`
instance FromJSON Charge where
    parseJSON (Object o) =
        Charge <$> (ChargeId <$> o .: "id")
               <*> o .: "object"
               <*> (fromSeconds <$> o .: "created")
               <*> o .: "livemode"
               <*> o .: "paid"
               <*> (Amount <$> o .: "amount")
               <*> o .: "currency"
               <*> o .: "refunded"
               <*> o .:? "card"
               <*> o .: "captured"
               <*> o .: "refunds"
               <*> o .:? "balance_transaction"
               <*> o .:? "failure_message"
               <*> o .:? "failure_code"
               <*> o .: "amount_refunded"
               <*> o .:? "customer"
               <*> o .:? "invoice"
               <*> o .:? "description"
               <*> o .:? "dispute"
               <*> o .: "metadata"
               <*> o .:? "statement_description"
               <*> o .:? "receipt_email"
               <*> o .:? "receipt_number"
    parseJSON _ = mzero

------------------------------------------------------------------------------
-- | Capture for `Charge`
newtype Capture = Capture { getCapture :: Bool }
  deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | `RefundId` for `Refund`
newtype RefundId =
  RefundId Text deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | `Refund` Object
data Refund = Refund {
      refundId                 :: RefundId
    , refundAmount             :: Int
    , refundCurrency           :: Currency
    , refundCreated            :: UTCTime
    , refundObject             :: Text
    , refundCharge             :: ChargeId
    , refundBalanceTransaction :: Maybe (Expandable TransactionId)
    , refundMetaData           :: MetaData
    } deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `Refund`
instance FromJSON Refund where
   parseJSON (Object o) =
        Refund <$> (RefundId <$> o .: "id")
               <*> o .: "amount"
               <*> o .: "currency"
               <*> (fromSeconds <$> o .: "created")
               <*> o .: "object"
               <*> o .: "charge"
               <*> o .:? "balance_transaction"
               <*> o .: "metadata"
   parseJSON _ = mzero

------------------------------------------------------------------------------
-- | `RefundApplicationFee`
newtype RefundApplicationFee =
  RefundApplicationFee { getRefundApplicationFee :: Bool }
  deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | `RefundReason`
data RefundReason
  = RefundDuplicate
  | RefundFraudulent
  | RefundRequestedByCustomer
  deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | `CustomerId` for a `Customer`
newtype CustomerId
  = CustomerId Text
  deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `CustomerId`
instance FromJSON CustomerId where
    parseJSON (String x)   = pure (CustomerId x)
    parseJSON _            = mzero

------------------------------------------------------------------------------
-- | `Customer` object
data Customer = Customer {
      customerObject         :: Text
    , customerCreated        :: UTCTime
    , customerId             :: CustomerId
    , customerLiveMode       :: Bool
    , customerDescription    :: Maybe Description
    , customerEmail          :: Maybe Email
    , customerDelinquent     :: Bool
    , customerSubscriptions  :: Maybe (StripeList Subscription)
    , customerDiscount       :: Maybe Discount
    , customerAccountBalance :: Int
    , customerCards          :: StripeList Card
    , customerCurrency       :: Maybe Currency
    , customerDefaultCard    :: Maybe (Expandable CardId)
    , customerMetaData       :: MetaData
    } | DeletedCustomer {
      deletedCustomer   :: Bool
    , deletedCustomerId :: CustomerId
  } deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `Customer`
instance FromJSON Customer where
  parseJSON (Object o)
        = (DeletedCustomer
           <$> o .: "deleted"
           <*> (CustomerId <$> o .: "id"))
           <|> (Customer
           <$> o .: "object"
           <*> (fromSeconds <$> o .: "created")
           <*> (CustomerId <$> o .: "id")
           <*> o .: "livemode"
           <*> o .:? "description"
           <*> (fmap Email <$> o .:? "email")
           <*> o .: "delinquent"
           <*> o .:? "subscriptions"
           <*> o .:? "discount"
           <*> o .: "account_balance"
           <*> o .: "cards"
           <*> o .:? "currency"
           <*> o .:? "default_card"
           <*> o .: "metadata")
  parseJSON o = typeMismatch "Customer" o

------------------------------------------------------------------------------
-- | AccountBalance for a `Customer`
newtype AccountBalance = AccountBalance Int
  deriving (Eq, Ord, Read, Show, Data, Typeable)

------------------------------------------------------------------------------
-- | CardId for a `Customer`
newtype CardId = CardId Text
  deriving (Eq, Ord, Read, Show, Data, Typeable)

------------------------------------------------------------------------------
-- | CardId for a `Recipient`
newtype RecipientCardId = RecipientCardId Text
  deriving (Eq, Ord, Read, Show, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `CardId`
instance FromJSON CardId where
   parseJSON (String x)   = pure $ CardId x
   parseJSON _ = mzero

------------------------------------------------------------------------------
-- | JSON Instance for `RecipientCardId`
instance FromJSON RecipientCardId where
   parseJSON (String x)   = pure $ RecipientCardId x
   parseJSON _ = mzero

------------------------------------------------------------------------------
-- | Number associated with a `Card`
newtype CardNumber     = CardNumber Text deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | Expiration Month for a `Card`
newtype ExpMonth       = ExpMonth Int deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | Expiration Year for a `Card`
newtype ExpYear        = ExpYear Int deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | CVC for a `Card`
newtype CVC            = CVC Text deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | City address for a `Card`
newtype AddressCity    = AddressCity Text deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | Country address for a `Card`
newtype AddressCountry = AddressCountry Text deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | Address Line One for a `Card`
newtype AddressLine1   = AddressLine1 Text deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | Address Line Two for a `Card`
newtype AddressLine2   = AddressLine2 Text deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | Address State for a `Card`
newtype AddressState   = AddressState Text deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | Address Zip Code for a `Card`
newtype AddressZip     = AddressZip Text deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | `IsVerified` `Recipients`
newtype IsVerified = IsVerified { getVerified :: Bool }
  deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | Credit / Debit Card Brand
data Brand = Visa
           | AMEX
           | MasterCard
           | Discover
           | JCB
           | DinersClub
           | Unknown
             deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `Brand`
instance FromJSON Brand where
   parseJSON (String "American Express") = pure AMEX
   parseJSON (String "MasterCard") = pure MasterCard
   parseJSON (String "Discover") = pure Discover
   parseJSON (String "JCB") = pure JCB
   parseJSON (String "Visa") = pure Visa
   parseJSON (String "DinersClub") = pure DinersClub
   parseJSON _ = mzero

------------------------------------------------------------------------------
-- | `Card` Object
data Card = Card {
      cardId                :: CardId
    , cardObject            :: Text
    , cardLastFour          :: Text
    , cardBrand             :: Brand
    , cardFunding           :: Text
    , cardExpMonth          :: ExpMonth
    , cardExpYear           :: ExpYear
    , cardFingerprint       :: Maybe Text
    , cardCountry           :: Maybe Text
    , cardName              :: Maybe Name
    , cardAddressLine1      :: Maybe AddressLine1
    , cardAddressLine2      :: Maybe AddressLine2
    , cardAddressCity       :: Maybe AddressCity
    , cardAddressState      :: Maybe AddressState
    , cardAddressZip        :: Maybe AddressZip
    , cardAddressCountry    :: Maybe AddressCountry
    , cardCVCCheck          :: Maybe Text
    , cardAddressLine1Check :: Maybe Text
    , cardAddressZipCheck   :: Maybe Text
    , cardCustomerId        :: Maybe (Expandable CustomerId)
    , cardMetaData          :: MetaData
    } deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | `RecipientCard` object
data RecipientCard = RecipientCard {
      recipientCardId                :: RecipientCardId
    , recipientCardLastFour          :: Text
    , recipientCardBrand             :: Brand
    , recipientCardFunding           :: Text
    , recipientCardExpMonth          :: ExpMonth
    , recipientCardExpYear           :: ExpYear
    , recipientCardFingerprint       :: Text
    , recipientCardCountry           :: Country
    , recipientCardName              :: Maybe Name
    , recipientCardAddressLine1      :: Maybe AddressLine1
    , recipientCardAddressLine2      :: Maybe AddressLine2
    , recipientCardAddressCity       :: Maybe AddressCity
    , recipientCardAddressState      :: Maybe AddressState
    , recipientCardAddressZip        :: Maybe AddressZip
    , recipientCardAddressCountry    :: Maybe AddressCountry
    , recipientCardCVCCheck          :: Maybe Text
    , recipientCardAddressLine1Check :: Maybe Text
    , recipientCardAddressZipCheck   :: Maybe Text
    , recipientCardRecipientId       :: Maybe (Expandable RecipientId)
} deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `Card`
instance FromJSON Card where
    parseJSON (Object o) =
        Card <$> (CardId <$> o .: "id")
             <*> o .: "object"
             <*> o .: "last4"
             <*> o .: "brand"
             <*> o .: "funding"
             <*> (ExpMonth <$> o .: "exp_month")
             <*> (ExpYear <$> o .: "exp_year")
             <*> o .:? "fingerprint"
             <*> o .:? "country"
             <*> o .:? "name"
             <*> (fmap AddressLine1 <$> o .:? "address_line1")
             <*> (fmap AddressLine2 <$> o .:? "address_line2")
             <*> (fmap AddressCity <$> o .:? "address_city")
             <*> (fmap AddressState <$> o .:? "address_state")
             <*> (fmap AddressZip <$> o .:? "address_zip")
             <*> (fmap AddressCountry <$> o .:? "address_country")
             <*> o .:? "cvc_check"
             <*> o .:? "address_line1_check"
             <*> o .:? "address_zip_check"
             <*> o .:? "customer"
             <*> o .: "metadata"
    parseJSON _ = mzero

------------------------------------------------------------------------------
-- | JSON Instance for `RecipientCard`
instance FromJSON RecipientCard where
    parseJSON (Object o) =
       RecipientCard
             <$> (RecipientCardId <$> o .: "id")
             <*> o .: "last4"
             <*> o .: "brand"
             <*> o .: "funding"
             <*> (ExpMonth <$> o .: "exp_month")
             <*> (ExpYear <$> o .: "exp_year")
             <*> o .: "fingerprint"
             <*> (Country <$> o .: "country")
             <*> o .:? "name"
             <*> (fmap AddressLine1 <$> o .:? "address_line1")
             <*> (fmap AddressLine2 <$> o .:? "address_line2")
             <*> (fmap AddressCity <$> o .:? "address_city")
             <*> (fmap AddressState <$> o .:? "address_state")
             <*> (fmap AddressZip <$> o .:? "address_zip")
             <*> (fmap AddressCountry <$> o .:? "address_country")
             <*> o .:? "cvc_check"
             <*> o .:? "address_line1_check"
             <*> o .:? "address_zip_check"
             <*> o .:? "recipient"
    parseJSON _ = mzero


------------------------------------------------------------------------------
-- | `NewCard` contains the data needed to create a new `Card`
data NewCard = NewCard
    { newCardCardNumber     :: CardNumber
    , newCardExpMonth       :: ExpMonth
    , newCardExpYear        :: ExpYear
    , newCardCVC            :: Maybe CVC
    , newCardName           :: Maybe Name
    , newCardAddressLine1   :: Maybe AddressLine1
    , newCardAddressLine2   :: Maybe AddressLine2
    , newCardAddressCity    :: Maybe AddressCity
    , newCardAddressZip     :: Maybe AddressZip
    , newCardAddressState   :: Maybe AddressState
    , newCardAddressCountry :: Maybe AddressCountry
    }
    deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | create a `NewCard` with only the required fields
mkNewCard
    :: CardNumber
    -> ExpMonth
    -> ExpYear
    -> NewCard
mkNewCard
    cardNumber
    expMonth
    expYear
    = NewCard
    { newCardCardNumber     = cardNumber
    , newCardExpMonth       = expMonth
    , newCardExpYear        = expYear
    , newCardCVC            = Nothing
    , newCardName           = Nothing
    , newCardAddressLine1   = Nothing
    , newCardAddressLine2   = Nothing
    , newCardAddressCity    = Nothing
    , newCardAddressZip     = Nothing
    , newCardAddressState   = Nothing
    , newCardAddressCountry = Nothing
    }

------------------------------------------------------------------------------
-- | set the `DefaultCard`
data DefaultCard = DefaultCard { getDefaultCard :: CardId }
    deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | `SubscriptionId` for a `Subscription`
newtype SubscriptionId = SubscriptionId { getSubscriptionId :: Text }
    deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `SubscriptionId`
instance FromJSON SubscriptionId where
    parseJSON (String x)   = pure (SubscriptionId x)
    parseJSON _            = mzero

------------------------------------------------------------------------------
-- | Subscription Object
data Subscription = Subscription {
      subscriptionId                    :: SubscriptionId
    , subscriptionPlan                  :: Plan
    , subscriptionObject                :: Text
    , subscriptionStart                 :: UTCTime
    , subscriptionStatus                :: SubscriptionStatus
    , subscriptionCustomerId            :: Expandable CustomerId
    , subscriptionCancelAtPeriodEnd     :: Bool
    , subscriptionCurrentPeriodStart    :: UTCTime
    , subscriptionCurrentPeriodEnd      :: UTCTime
    , subscriptionEndedAt               :: Maybe UTCTime
    , subscriptionTrialStart            :: Maybe UTCTime
    , subscriptionTrialEnd              :: Maybe UTCTime
    , subscriptionCanceledAt            :: Maybe UTCTime
    , subscriptionQuantity              :: Quantity
    , subscriptionApplicationFeePercent :: Maybe Double
    , subscriptionDiscount              :: Maybe Discount
    , subscriptionMetaData              :: MetaData
    , subscriptionTaxPercent            :: Maybe Double
} deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `Subscription`
instance FromJSON Subscription where
   parseJSON (Object o) =
       Subscription <$> (SubscriptionId <$> o .: "id")
                    <*> o .: "plan"
                    <*> o .: "object"
                    <*> (fromSeconds <$> o .: "start")
                    <*> o .: "status"
                    <*> o .: "customer"
                    <*> o .: "cancel_at_period_end"
                    <*> (fromSeconds <$> o .: "current_period_start")
                    <*> (fromSeconds <$> o .: "current_period_end")
                    <*> (fmap fromSeconds <$> o .:? "ended_at")
                    <*> (fmap fromSeconds <$> o .:? "trial_start")
                    <*> (fmap fromSeconds <$> o .:? "trial_end")
                    <*> (fmap fromSeconds <$> o .:? "canceled_at")
                    <*> (Quantity <$> o .:  "quantity")
                    <*> o .:? "application_fee_percent"
                    <*> o .:? "discount"
                    <*> o .: "metadata"
                    <*> o .:? "tax_percent"
   parseJSON _ = mzero

------------------------------------------------------------------------------
-- | Status of a `Subscription`
data SubscriptionStatus =
          Trialing
        | Active
        | PastDue
        | Canceled
        | UnPaid
        deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `SubscriptionStatus`
instance FromJSON SubscriptionStatus where
   parseJSON (String "trialing") = pure Trialing
   parseJSON (String "active")   = pure Active
   parseJSON (String "past_due") = pure PastDue
   parseJSON (String "canceled") = pure Canceled
   parseJSON (String "unpaid")   = pure UnPaid
   parseJSON _                   = mzero

------------------------------------------------------------------------------
-- | `TaxPercent` for a `Subscription`
newtype TaxPercent = TaxPercent Double deriving (Read, Show, Eq, Ord, Data, Typeable)


------------------------------------------------------------------------------
-- | `PlanId` for a `Plan`
newtype PlanId = PlanId Text deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | Plan object
data Plan = Plan {
      planInterval        :: Interval
    , planName            :: Text
    , planCreated         :: UTCTime
    , planAmount          :: Int
    , planCurrency        :: Currency
    , planId              :: PlanId
    , planObject          :: Text
    , planLiveMode        :: Bool
    , planIntervalCount   :: Maybe Int -- optional, max of 1 year intervals allowed, default 1
    , planTrialPeriodDays :: Maybe Int
    , planMetaData        :: MetaData
    , planDescription     :: Maybe StatementDescription
} deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `Plan`
instance FromJSON Plan where
   parseJSON (Object o) =
        Plan <$> o .: "interval"
             <*> o .: "name"
             <*> (fromSeconds <$> o .: "created")
             <*> o .: "amount"
             <*> o .: "currency"
             <*> (PlanId <$> o .: "id")
             <*> o .: "object"
             <*> o .: "livemode"
             <*> o .:? "interval_count"
             <*> o .:? "trial_period_days"
             <*> o .: "metadata"
             <*> o .:? "statement_description"
   parseJSON _ = mzero

------------------------------------------------------------------------------
-- | `TrialPeriod` for a Plan
newtype TrialPeriod = TrialPeriod UTCTime deriving (Show, Eq)

------------------------------------------------------------------------------
-- | `TrialEnd` for a Plan
newtype TrialEnd = TrialEnd UTCTime
    deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | Interval for `Plan`s
data Interval = Day | Week | Month | Year deriving (Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `Interval`
instance FromJSON Interval where
   parseJSON (String "day") = pure Day
   parseJSON (String "week") = pure Week
   parseJSON (String "month") = pure Month
   parseJSON (String "year") = pure Year
   parseJSON _ = mzero

------------------------------------------------------------------------------
-- | `Show` instance for `Interval`
instance Show Interval where
    show Day   = "day"
    show Week  = "week"
    show Month = "month"
    show Year  = "year"

------------------------------------------------------------------------------
-- | `Read` instance for `Interval`
instance Read Interval where
  readPrec =
    do (R.String s) <- lexP
       case s of
         "day"   -> return Day
         "week"  -> return Week
         "month" -> return Month
         "year"  -> return Year
         _       -> pfail

------------------------------------------------------------------------------
-- | `Coupon` Duration
data Duration = Forever | Once | Repeating deriving (Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | `Show` instance for `Duration`
instance Show Duration where
    show Forever   = "forever"
    show Once      = "once"
    show Repeating = "repeating"

------------------------------------------------------------------------------
-- | `Read` instance for `Duration`
instance Read Duration where
  readPrec =
    do (R.String s) <- lexP
       case s of
         "forever"   -> return Forever
         "once"      -> return Once
         "repeating" -> return Repeating
         _           -> pfail

------------------------------------------------------------------------------
-- | JSON Instance for `Duration`
instance FromJSON Duration where
   parseJSON (String x)
       | x == "forever"   = pure Forever
       | x == "once"      = pure Once
       | x == "repeating" = pure Repeating
   parseJSON _ = mzero

------------------------------------------------------------------------------
-- | `Coupon` Object
data Coupon = Coupon {
      couponId               :: CouponId
    , couponCreated          :: UTCTime
    , couponPercentOff       :: Maybe Int
    , couponAmountOff        :: Maybe Int
    , couponCurrency         :: Maybe Currency
    , couponLiveMode         :: Bool
    , couponDuration         :: Duration
    , couponRedeemBy         :: Maybe UTCTime
    , couponMaxRedemptions   :: Maybe Int
    , couponTimesRedeemed    :: Maybe Int
    , couponDurationInMonths :: Maybe Int
    , couponValid            :: Bool
    , couponMetaData         :: MetaData
    } deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `Coupon`
instance FromJSON Coupon where
   parseJSON (Object o) =
        Coupon <$> (CouponId <$> o .: "id")
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
               <*> o .: "metadata"
   parseJSON _ = mzero

------------------------------------------------------------------------------
-- | `CouponId` for a `Coupon`
newtype CouponId = CouponId Text deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | `AmountOff` for a `Coupon`
newtype AmountOff = AmountOff Int deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | `MaxRedemptions` for a `Coupon`
newtype MaxRedemptions = MaxRedemptions Int deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | `PercentOff` for a `Coupon`
newtype PercentOff = PercentOff Int deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | `RedeemBy` date for a `Coupon`
newtype RedeemBy = RedeemBy UTCTime deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | `DurationInMonths` for a `Coupon`
newtype DurationInMonths = DurationInMonths Int deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | `IntervalCount` for a `Coupon`
newtype IntervalCount   = IntervalCount Int deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | `TrialPeriodDays` for a `Coupon`
newtype TrialPeriodDays = TrialPeriodDays Int deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | Amount representing a monetary value.
-- Stripe represents pennies as whole numbers
-- i.e. 100 = $1
newtype Amount = Amount { getAmount :: Int }
  deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | `Discount` for `Coupon`
data Discount = Discount {
      discountCoupon       :: Coupon
    , discountStart        :: UTCTime
    , discountEnd          :: Maybe UTCTime
    , discountCustomer     :: Expandable CustomerId
    , discountObject       :: Text
    , discountSubscription :: Maybe SubscriptionId
} deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `Discount`
instance FromJSON Discount where
    parseJSON (Object o) =
        Discount <$> o .: "coupon"
                 <*> (fromSeconds <$> o .: "start")
                 <*> (fmap fromSeconds <$> o .:? "end")
                 <*> o .: "customer"
                 <*> o .: "object"
                 <*> (fmap SubscriptionId <$> o .:? "subscription")
    parseJSON _ = mzero

------------------------------------------------------------------------------
-- | `Invoice` for a `Coupon`
newtype InvoiceId =
    InvoiceId Text
  deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `InvoiceId`
instance FromJSON InvoiceId where
   parseJSON (String x)   = pure $ InvoiceId x
   parseJSON _ = mzero

------------------------------------------------------------------------------
-- | `Invoice` Object
data Invoice = Invoice {
      invoiceDate                 :: UTCTime
    , invoiceId                   :: Maybe InvoiceId -- ^ If upcoming no ID will exist
    , invoicePeriodStart          :: UTCTime
    , invoicePeriodEnd            :: UTCTime
    , invoiceLineItems            :: StripeList InvoiceLineItem
    , invoiceSubTotal             :: Int
    , invoiceTotal                :: Int
    , invoiceCustomer             :: Expandable CustomerId
    , invoiceObject               :: Text
    , invoiceAttempted            :: Bool
    , invoiceClosed               :: Bool
    , invoiceForgiven             :: Bool
    , invoicePaid                 :: Bool
    , invoiceLiveMode             :: Bool
    , invoiceAttemptCount         :: Int
    , invoiceAmountDue            :: Int
    , invoiceCurrency             :: Currency
    , invoiceStartingBalance      :: Int
    , invoiceEndingBalance        :: Maybe Int
    , invoiceNextPaymentAttempt   :: Maybe UTCTime
    , invoiceWebHooksDeliveredAt  :: Maybe UTCTime
    , invoiceCharge               :: Maybe (Expandable ChargeId)
    , invoiceDiscount             :: Maybe Discount
    , invoiceApplicateFee         :: Maybe FeeId
    , invoiceSubscription         :: Maybe SubscriptionId
    , invoiceStatementDescription :: Maybe StatementDescription
    , invoiceDescription          :: Maybe Description
    , invoiceMetaData             :: MetaData
} deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `Invoice`
instance FromJSON Invoice where
   parseJSON (Object o) =
       Invoice <$> (fromSeconds <$> o .: "date")
               <*> (fmap InvoiceId <$> o .:? "id")
               <*> (fromSeconds <$> o .: "period_start")
               <*> (fromSeconds <$> o .: "period_end")
               <*> o .: "lines"
               <*> o .: "subtotal"
               <*> o .: "total"
               <*> o .: "customer"
               <*> o .: "object"
               <*> o .: "attempted"
               <*> o .: "closed"
               <*> o .: "forgiven"
               <*> o .: "paid"
               <*> o .: "livemode"
               <*> o .: "attempt_count"
               <*> o .: "amount_due"
               <*> o .: "currency"
               <*> o .: "starting_balance"
               <*> o .:? "ending_balance"
               <*> (fmap fromSeconds <$> o .:? "next_payment_attempt")
               <*> (fmap fromSeconds <$> o .: "webhooks_delivered_at")
               <*> o .:? "charge"
               <*> o .:? "discount"
               <*> (fmap FeeId <$> o .:? "application_fee")
               <*> (fmap SubscriptionId <$> o .: "subscription")
               <*> o .:? "statement_description"
               <*> o .:? "description"
               <*> o .: "metadata"
   parseJSON _ = mzero

------------------------------------------------------------------------------
-- | `InvoiceItemId` for `InvoiceItem`
newtype InvoiceItemId
    = InvoiceItemId Text
      deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | `InvoiceItem` object
data InvoiceItem = InvoiceItem {
      invoiceItemObject       :: Text
    , invoiceItemId           :: InvoiceItemId
    , invoiceItemDate         :: UTCTime
    , invoiceItemAmount       :: Int
    , invoiceItemLiveMode     :: Bool
    , invoiceItemProration    :: Bool
    , invoiceItemCurrency     :: Currency
    , invoiceItemCustomer     :: Expandable CustomerId
    , invoiceItemDescription  :: Maybe Description
    , invoiceItemInvoice      :: Maybe (Expandable InvoiceId)
    , invoiceItemQuantity     :: Maybe Quantity
    , invoiceItemSubscription :: Maybe SubscriptionId
    , invoiceItemMetaData     :: MetaData
    } deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `InvoiceItem`
instance FromJSON InvoiceItem where
   parseJSON (Object o) =
       InvoiceItem <$> o .: "object"
                   <*> (InvoiceItemId <$> o .: "id")
                   <*> (fromSeconds <$> o .: "date")
                   <*> o .: "amount"
                   <*> o .: "livemode"
                   <*> o .: "proration"
                   <*> o .: "currency"
                   <*> o .: "customer"
                   <*> o .:? "description"
                   <*> o .:? "invoice"
                   <*> (fmap Quantity <$> o .:? "quantity")
                   <*> o .:? "subscription"
                   <*> o .: "metadata"
   parseJSON _ = mzero

------------------------------------------------------------------------------
-- | `InvoiceLineItemId` for an `InvoiceLineItem`
newtype InvoiceLineItemId =
    InvoiceLineItemId Text deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | Type of `InvoiceItem`
data InvoiceLineItemType
    = InvoiceItemType |
     SubscriptionItemType
      deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `InvoiceLineItemType`
instance FromJSON InvoiceLineItemType where
   parseJSON (String "invoiceitem")  = pure InvoiceItemType
   parseJSON (String "subscription") = pure SubscriptionItemType
   parseJSON _ = mzero

------------------------------------------------------------------------------
-- | `InvoiceLineItem` Object
data InvoiceLineItem = InvoiceLineItem {
      invoiceLineItemId          :: InvoiceLineItemId
    , invoiceLineItemObject      :: Text
    , invoiceLineItemType        :: InvoiceLineItemType
    , invoiceLineItemLiveMode    :: Bool
    , invoiceLineItemAmount      :: Int
    , invoiceLineItemCurrency    :: Currency
    , invoiceLineItemProration   :: Bool
    , invoiceLineItemPeriod      :: Period
    , invoiceLineItemQuantity    :: Maybe Quantity
    , invoiceLineItemPlan        :: Maybe Plan
    , invoiceLineItemDescription :: Maybe Description
    , invoiceLineItemMetaData    :: MetaData
  } deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | Period for an `InvoiceLineItem`
data Period = Period {
      start :: UTCTime
    , end   :: UTCTime
    } deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `Period`
instance FromJSON Period where
   parseJSON (Object o) =
       Period <$> (fromSeconds <$> o .: "start")
              <*> (fromSeconds <$> o .: "end")
   parseJSON _ = mzero

------------------------------------------------------------------------------
-- | JSON Instance for `InvoiceLineItem`
instance FromJSON InvoiceLineItem where
   parseJSON (Object o) =
       InvoiceLineItem <$> (InvoiceLineItemId <$> o .: "id")
                       <*> o .: "object"
                       <*> o .: "type"
                       <*> o .: "livemode"
                       <*> o .: "amount"
                       <*> o .: "currency"
                       <*> o .: "proration"
                       <*> o .: "period"
                       <*> (fmap Quantity <$> o .:? "quantity")
                       <*> o .:? "plan"
                       <*> o .:? "description"
                       <*> o .: "metadata"
   parseJSON _ = mzero


------------------------------------------------------------------------------
-- | `Closed` - invoice closed or not
newtype Closed =
  Closed { getClosed :: Bool }
  deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | `Forgiven` - invoice forgiven or not
newtype Forgiven =
  Forgiven { getForgiven :: Bool }
  deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | Status of a `Dispute`
data DisputeStatus
    = WarningNeedsResponse
    | WarningUnderReview
    | NeedsResponse
    | UnderReview
    | ChargeRefunded
    | Won
    | Lost
    deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `DisputeReason`
instance FromJSON DisputeReason where
   parseJSON (String "duplicate") = pure Duplicate
   parseJSON (String "fraudulent") = pure Fraudulent
   parseJSON (String "subscription_canceled") = pure SubscriptionCanceled
   parseJSON (String "product_unacceptable") = pure ProductUnacceptable
   parseJSON (String "product_not_received") = pure ProductNotReceived
   parseJSON (String "credit_not_processed") = pure CreditNotProcessed
   parseJSON (String "general") = pure General
   parseJSON _ = mzero

------------------------------------------------------------------------------
-- | Reason of a `Dispute`
data DisputeReason
    = Duplicate
    | Fraudulent
    | SubscriptionCanceled
    | ProductUnacceptable
    | ProductNotReceived
    | Unrecognized
    | CreditNotProcessed
    | General
      deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `DisputeStatus`
instance FromJSON DisputeStatus where
   parseJSON (String "needs_response") = pure NeedsResponse
   parseJSON (String "warning_needs_response") = pure WarningNeedsResponse
   parseJSON (String "warning_under_review") = pure WarningUnderReview
   parseJSON (String "under_review") = pure UnderReview
   parseJSON (String "charge_refunded") = pure ChargeRefunded
   parseJSON (String "won") = pure Won
   parseJSON (String "lost") = pure Lost
   parseJSON _ = mzero

------------------------------------------------------------------------------
-- | `Dispute` Object
data Dispute = Dispute {
      disputeChargeId            :: Expandable ChargeId
    , disputeAmount              :: Int
    , disputeCreated             :: UTCTime
    , disputeStatus              :: DisputeStatus
    , disputeLiveMode            :: Bool
    , disputeCurrency            :: Currency
    , disputeObject              :: Text
    , disputeReason              :: DisputeReason
    , disputeIsChargeRefundable  :: Bool
    , disputeBalanceTransactions :: [BalanceTransaction]
    , disputeEvidenceDueBy       :: UTCTime
    , disputeEvidence            :: Maybe Evidence
    , disputeMetaData            :: MetaData
    } deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | `Evidence` associated with a `Dispute`
newtype Evidence = Evidence Text deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `Dispute`
instance FromJSON Dispute where
    parseJSON (Object o) =
        Dispute <$> o .: "charge"
                <*> o .: "amount"
                <*> (fromSeconds <$> o .: "created")
                <*> o .: "status"
                <*> o .: "livemode"
                <*> o .: "currency"
                <*> o .: "object"
                <*> o .: "reason"
                <*> o .: "is_charge_refundable"
                <*> o .: "balance_transactions"
                <*> (fromSeconds <$> o .: "evidence_due_by")
                <*> (fmap Evidence <$> o .:? "evidence")
                <*> o .: "metadata"
    parseJSON _ = mzero

------------------------------------------------------------------------------
-- | `TransferId`
newtype TransferId =
  TransferId Text deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | Status of a `Transfer`
data TransferStatus =
    TransferPaid
  | TransferPending
  | TransferCanceled
  | TransferFailed
  deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | Type of a `Transfer`
data TransferType =
    CardTransfer
  | BankAccountTransfer
    deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `TransferType`
instance FromJSON TransferType where
    parseJSON (String "card")         = pure CardTransfer
    parseJSON (String "bank_account") = pure BankAccountTransfer
    parseJSON _                       = mzero

------------------------------------------------------------------------------
-- | JSON Instance for `TransferStatus`
instance FromJSON TransferStatus where
    parseJSON (String "paid")     = pure TransferPaid
    parseJSON (String "pending")  = pure TransferPending
    parseJSON (String "canceled") = pure TransferCanceled
    parseJSON _                   = mzero

------------------------------------------------------------------------------
-- | `Transfer` Object
data Transfer = Transfer {
      transferId                   :: TransferId
     , transferObject               :: Text
     , transferCreated              :: UTCTime
     , transferDate                 :: UTCTime
     , transferLiveMode             :: Bool
     , transferAmount               :: Int
     , transferCurrency             :: Currency
     , transferStatus               :: TransferStatus
     , transferType                 :: TransferType
     , transferBalanceTransaction   :: Expandable TransactionId
     , transferDescription          :: Maybe Description
     , transferBankAccount          :: Maybe BankAccount
     , transferFailureMessage       :: Maybe Text
     , transferFailureCode          :: Maybe Text
     , transferStatementDescription :: Maybe StatementDescription
     , transferRecipient            :: Maybe (Expandable RecipientId)
     , transferMetaData             :: MetaData
} deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `Transfer`
instance FromJSON Transfer where
    parseJSON (Object o) =
        Transfer <$> (TransferId <$> o .: "id")
                    <*> o .: "object"
                    <*> (fromSeconds <$> o .: "created")
                    <*> (fromSeconds <$> o .: "date")
                    <*> o .: "livemode"
                    <*> o .: "amount"
                    <*> o .: "currency"
                    <*> o .: "status"
                    <*> o .: "type"
                    <*> o .: "balance_transaction"
                    <*> o .:? "description"
                    <*> o .:? "bank_account"
                    <*> o .:? "failure_message"
                    <*> o .:? "failure_code"
                    <*> o .:? "statement_description"
                    <*> o .:? "recipient"
                    <*> o .: "metadata"
    parseJSON _ = mzero

------------------------------------------------------------------------------
-- | `BankAccount` Object
data BankAccount = BankAccount {
      bankAccountId          :: BankAccountId
    , bankAccountObject      :: Text
    , bankAccountLast4       :: Text
    , bankAccountCountry     :: Country
    , bankAccountCurrency    :: Currency
    , bankAccountStatus      :: Maybe BankAccountStatus
    , bankAccountFingerprint :: Maybe Text
    , bankAccountName        :: Text
} deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | `BankAccount` JSON Instance
instance FromJSON BankAccount where
   parseJSON (Object o) =
     BankAccount <$> (BankAccountId <$> o .: "id")
                 <*> o .: "object"
                 <*> o .: "last4"
                 <*> (Country <$> o .: "country")
                 <*> o .: "currency"
                 <*> o .:? "status"
                 <*> o .:? "fingerprint"
                 <*> o .: "bank_name"
   parseJSON _ = mzero

------------------------------------------------------------------------------
-- | `BankAccountId` for `BankAccount`
newtype BankAccountId = BankAccountId Text
                        deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | `BankAccountStatus` Object
data BankAccountStatus =
  New | Validated | Verified | Errored
  deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | `BankAccountStatus` JSON instance
instance FromJSON BankAccountStatus where
   parseJSON (String "new") = pure $ New
   parseJSON (String "validated") = pure Validated
   parseJSON (String "verified") = pure Verified
   parseJSON (String "errored") = pure Errored
   parseJSON _ = mzero

------------------------------------------------------------------------------
-- | Routing Number for Bank Account
newtype RoutingNumber =
  RoutingNumber Text deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | Country
newtype Country       =
  Country Text deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | Account Number of a Bank Account
newtype AccountNumber =
  AccountNumber Text deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | create a new `BankAccount`
data NewBankAccount = NewBankAccount
    { newBankAccountCountry       :: Country
    , newBankAccountRoutingNumber :: RoutingNumber
    , newBankAccountAccountNumber :: AccountNumber
    }
    deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | Recipients

------------------------------------------------------------------------------
-- | `FirstName` of a `Recipient`
newtype FirstName = FirstName Text deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | `LastName` of a `Recipient`
newtype LastName = LastName Text deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | Middle Initial of a `Recipient`
type MiddleInitial = Char

------------------------------------------------------------------------------
-- | `RecipientId` for a `Recipient`
newtype RecipientId =
      RecipientId Text
  deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `RecipientId`
instance FromJSON RecipientId where
   parseJSON (String x)   = pure $ RecipientId x
   parseJSON _ = mzero

------------------------------------------------------------------------------
-- | `TaxID`
newtype TaxID  = TaxID { getTaxID :: Text }
  deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | Type of `Recipient`
data RecipientType =
    Individual
  | Corporation deriving (Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | `Show` instance for `RecipientType`
instance Show RecipientType where
    show Individual  = "individual"
    show Corporation = "corporation"

------------------------------------------------------------------------------
-- | `Read` instance for `RecipientType`
instance Read RecipientType where
  readPrec =
    do (R.String s) <- lexP
       case s of
         "individual"  -> return Individual
         "corporation" -> return Corporation
         _             -> pfail

------------------------------------------------------------------------------
-- | JSON Instance for `RecipientType`
instance FromJSON RecipientType where
   parseJSON (String "individual")  = pure Individual
   parseJSON (String "corporation") = pure Corporation
   parseJSON _ = mzero

------------------------------------------------------------------------------
-- | Recipient Object
data Recipient = Recipient {
      recipientId            :: RecipientId
    , recipientObject        :: Text
    , recipientCreated       :: UTCTime
    , recipientLiveMode      :: Bool
    , recipientType          :: RecipientType
    , recipientDescription   :: Maybe Description
    , recipientEmail         :: Maybe Email
    , recipientName          :: Name
    , recipientVerified      :: Bool
    , recipientActiveAccount :: Maybe BankAccount
    , recipientCards         :: StripeList RecipientCard
    , recipientDefaultCard   :: Maybe (Expandable RecipientCardId)
 } | DeletedRecipient {
    deletedRecipient   :: Maybe Bool
  , deletedRecipientId :: RecipientId
 } deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `Recipient`
instance FromJSON Recipient where
   parseJSON (Object o) =
      (Recipient <$> (RecipientId <$> o .: "id")
                 <*> o .: "object"
                 <*> (fromSeconds <$> o .: "created")
                 <*> o .: "livemode"
                 <*> o .: "type"
                 <*> o .:? "description"
                 <*> (fmap Email <$> o .:? "email")
                 <*> o .: "name"
                 <*> o .: "verified"
                 <*> o .:? "active_account"
                 <*> o .: "cards"
                 <*> o .:? "default_card"
      )
      <|> DeletedRecipient
                 <$> o .:? "deleted"
                 <*> (RecipientId <$> o .: "id")

   parseJSON _ = mzero

------------------------------------------------------------------------------
-- | `PlanId` for a `Plan`
newtype ApplicationFeeId = ApplicationFeeId Text deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | ApplicationFee Object
data ApplicationFee = ApplicationFee {
      applicationFeeId                 :: ApplicationFeeId
    , applicationFeeObjecet            :: Text
    , applicationFeeCreated            :: UTCTime
    , applicationFeeLiveMode           :: Bool
    , applicationFeeAmount             :: Int
    , applicationFeeCurrency           :: Currency
    , applicationFeeRefunded           :: Bool
    , applicationFeeAmountRefunded     :: Int
    , applicationFeeRefunds            :: StripeList Refund
    , applicationFeeBalanceTransaction :: Expandable TransactionId
    , applicationFeeAccountId          :: Expandable AccountId
    , applicationFeeApplicationId      :: ApplicationId
    , applicationFeeChargeId           :: Expandable ChargeId
    , applicationFeeMetaData           :: MetaData
} deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | ApplicationFeePercent
newtype ApplicationFeePercent = ApplicationFeePercent Double
  deriving (Read, Show, Eq, Ord, Data, Typeable)


------------------------------------------------------------------------------
-- | ApplicationFeeAmount
newtype ApplicationFeeAmount = ApplicationFeeAmount Integer
  deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | `ApplicationId` object
newtype ApplicationId =
  ApplicationId Text deriving (Read, Show, Eq, Ord, Data, Typeable, FromJSON)

------------------------------------------------------------------------------
-- | JSON Instance for `ApplicationFee`
instance FromJSON ApplicationFee where
   parseJSON (Object o) =
       ApplicationFee <$> (ApplicationFeeId <$> o .: "id")
                      <*> o .: "object"
                      <*> (fromSeconds <$> o .: "created")
                      <*> o .: "livemode"
                      <*> o .: "amount"
                      <*> o .: "currency"
                      <*> o .: "refunded"
                      <*> o .: "amount_refunded"
                      <*> o .: "refunds"
                      <*> o .: "balance_transaction"
                      <*> o .: "account"
                      <*> (ApplicationId <$> o .: "application")
                      <*> o .: "charge"
                      <*> o .: "metadata"
   parseJSON _ = mzero

------------------------------------------------------------------------------
-- | `FeeId` for objects with Fees
newtype FeeId =
  FeeId Text
  deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | Application Fee Refunds
data ApplicationFeeRefund = ApplicationFeeRefund {
       applicationFeeRefundId                 :: RefundId
     , applicationFeeRefundAmount             :: Int
     , applicationFeeRefundCurrency           :: Currency
     , applicationFeeRefundCreated            :: UTCTime
     , applicationFeeRefundObject             :: Text
     , applicationFeeRefundBalanceTransaction :: Maybe (Expandable TransactionId)
     , applicationFeeRefundFee                :: FeeId
     , applicationFeeRefundMetaData           :: MetaData
     } deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `ApplicationFeeRefund`
instance FromJSON ApplicationFeeRefund where
    parseJSON (Object o) = ApplicationFeeRefund
              <$> (RefundId <$> o .: "id")
              <*> o .: "amount"
              <*> o .: "currency"
              <*> (fromSeconds <$> o .: "created")
              <*> o .: "object"
              <*> o .:? "balance_transaction"
              <*> (FeeId <$> o .: "fee")
              <*> o .: "metadata"
    parseJSON _ = mzero

------------------------------------------------------------------------------
-- | `AccountId` of an `Account`
newtype AccountId
  = AccountId Text
  deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `AccountId`
instance FromJSON AccountId where
   parseJSON (String aid) = pure $ AccountId aid
   parseJSON _ = mzero

------------------------------------------------------------------------------
-- | `Account` Object
data Account = Account {
       accountId                   :: AccountId
     , accountEmail                :: Email
     , accountStatementDescriptor  :: Maybe Description
     , accountDisplayName          :: Maybe Text
     , accountTimeZone             :: Text
     , accountDetailsSubmitted     :: Bool
     , accountChargeEnabled        :: Bool
     , accountTransferEnabled      :: Bool
     , accountCurrenciesSupported  :: [Currency]
     , accountDefaultCurrency      :: Currency
     , accountCountry              :: Text
     , accountObject               :: Text
     , accountBusinessName         :: Maybe Text
     , accountBusinessURL          :: Maybe Text
     , accountBusinessLogo         :: Maybe Text
     , accountSupportPhone         :: Maybe Text
} deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `Account`
instance FromJSON Account where
   parseJSON (Object o) =
       Account <$> (AccountId <$> o .:  "id")
               <*> (Email <$> o .:  "email")
               <*> o .:? "statement_descriptor"
               <*> o .:  "display_name"
               <*> o .:  "timezone"
               <*> o .:  "details_submitted"
               <*> o .:  "charge_enabled"
               <*> o .:  "transfer_enabled"
               <*> o .:  "currencies_supported"
               <*> o .:  "default_currency"
               <*> o .:  "country"
               <*> o .:  "object"
               <*> o .:?  "business_name"
               <*> o .:?  "business_url"
               <*> o .:?  "business_logo"
               <*> o .:?  "support_phone"
   parseJSON _ = mzero

------------------------------------------------------------------------------
-- | `Balance` Object
data Balance = Balance {
      balancePending   :: [BalanceAmount]
    , balanceAvailable :: [BalanceAmount]
    , balanceLiveMode  :: Bool
    , balanceObject    :: Text
    } deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `Balance`
instance FromJSON Balance where
   parseJSON (Object o) =
       Balance <$> o .: "pending"
               <*> o .: "available"
               <*> o .: "livemode"
               <*> o .: "object"
   parseJSON _ = mzero

------------------------------------------------------------------------------
-- | `BalanceAmount` Object
data BalanceAmount = BalanceAmount {
      balanceAmount   :: Int
    , balanceCurrency :: Currency
    } deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `BalanceAmount`
instance FromJSON BalanceAmount where
   parseJSON (Object o) =
       BalanceAmount <$> o .: "amount"
                     <*> o .: "currency"
   parseJSON _ = mzero

------------------------------------------------------------------------------
-- | `BalanceTransaction` Object
data BalanceTransaction = BalanceTransaction {
      balanceTransactionId             :: TransactionId
    , balanceTransactionObject         :: Text
    , balanceTransactionAmount         :: Int
    , balanceTransactionCurrency       :: Currency
    , balanceTransactionNet            :: Int
    , balanceTransactionType           :: TransactionType
    , balanceTransactionCreated        :: UTCTime
    , balanceTransactionAvailableOn    :: UTCTime
    , balanceTransactionStatus         :: Text
    , balanceTransactionFee            :: Int
    , balanceTransactionFeeDetails     :: [FeeDetails]
    , balanceTransactionFeeSource      :: Expandable ChargeId
    , balanceTransactionFeeDescription :: Maybe Description
    } deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `BalanceTransaction`
instance FromJSON BalanceTransaction where
   parseJSON (Object o) =
       BalanceTransaction <$> (TransactionId <$> o .: "id")
                          <*> o .: "object"
                          <*> o .: "amount"
                          <*> o .: "currency"
                          <*> o .: "net"
                          <*> o .: "type"
                          <*> (fromSeconds <$> o .: "created")
                          <*> (fromSeconds <$> o .: "available_on")
                          <*> o .: "status"
                          <*> o .: "fee"
                          <*> o .: "fee_details"
                          <*> o .: "source"
                          <*> o .:? "description"
   parseJSON _ = mzero

------------------------------------------------------------------------------
-- | `TransactionId` of a `Transaction`
newtype TransactionId = TransactionId Text
                   deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `TransactionId`
instance FromJSON TransactionId where
    parseJSON (String x)   = pure (TransactionId x)
    parseJSON _            = mzero

------------------------------------------------------------------------------
-- | `FeeDetails` Object
data FeeDetails = FeeDetails {
      feeDetailsAmount   :: Int
    , feeDetailsCurrency :: Currency
    , feeType            :: Text
    , feeDescription     :: Maybe Description
    , feeApplication     :: Maybe Text
} deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `FeeDetails`
instance FromJSON FeeDetails where
   parseJSON (Object o) =
       FeeDetails <$> o .: "amount"
                  <*> o .: "currency"
                  <*> o .: "type"
                  <*> o .: "description"
                  <*> o .:? "application"
   parseJSON _ = mzero

------------------------------------------------------------------------------
-- | `Source` used for filtering `Balance` transactions. It should contain
-- an object Id such as a `ChargeId`
newtype Source a = Source { getSource :: a }
    deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | transaction type for `BalanceTransaction`
data TransactionType
  = ChargeTxn
  | RefundTxn
  | AdjustmentTxn
  | ApplicationFeeTxn
  | ApplicationFeeRefundTxn
  | TransferTxn
  | TransferCancelTxn
  | TransferFailureTxn
    deriving (Read, Show, Eq, Ord, Data, Typeable)

instance FromJSON TransactionType where
  parseJSON (String "charge")           = pure ChargeTxn
  parseJSON (String "refund")           = pure RefundTxn
  parseJSON (String "adjustment")       = pure AdjustmentTxn
  parseJSON (String "application_fee")  = pure ApplicationFeeTxn
  parseJSON (String "application_fee_refund") = pure ApplicationFeeRefundTxn
  parseJSON (String "transfer")         = pure TransferTxn
  parseJSON (String "transfer_cancel")  = pure TransferCancelTxn
  parseJSON (String "transfer_failure") = pure TransferFailureTxn
  parseJSON _                           = mzero

instance ToJSON TransactionType where
  toJSON ChargeTxn          = String "charge"
  toJSON RefundTxn          = String "refund"
  toJSON AdjustmentTxn      = String "adjustment"
  toJSON ApplicationFeeTxn  = String "application_fee"
  toJSON ApplicationFeeRefundTxn = String "application_fee_refund"
  toJSON TransferTxn        = String "transfer"
  toJSON TransferCancelTxn  = String "transfer_cancel"
  toJSON TransferFailureTxn = String "transfer_failure"

------------------------------------------------------------------------------
-- | `Event` Types
data EventType =
    AccountUpdatedEvent
  | AccountApplicationDeauthorizedEvent
  | ApplicationFeeCreatedEvent
  | ApplicationFeeRefundedEvent
  | BalanceAvailableEvent
  | ChargeSucceededEvent
  | ChargeFailedEvent
  | ChargeRefundedEvent
  | ChargeCapturedEvent
  | ChargeUpdatedEvent
  | ChargeDisputeCreatedEvent
  | ChargeDisputeUpdatedEvent
  | ChargeDisputeClosedEvent
  | ChargeDisputeFundsWithdrawnEvent
  | ChargeDisputeFundsReinstatedEvent
  | CustomerCreatedEvent
  | CustomerUpdatedEvent
  | CustomerDeletedEvent
  | CustomerCardCreatedEvent
  | CustomerCardUpdatedEvent
  | CustomerCardDeletedEvent
  | CustomerSubscriptionCreatedEvent
  | CustomerSubscriptionUpdatedEvent
  | CustomerSubscriptionDeletedEvent
  | CustomerSubscriptionTrialWillEndEvent
  | CustomerDiscountCreatedEvent
  | CustomerDiscountUpdatedEvent
  | CustomerDiscountDeletedEvent
  | InvoiceCreatedEvent
  | InvoiceUpdatedEvent
  | InvoicePaymentSucceededEvent
  | InvoicePaymentFailedEvent
  | InvoiceItemCreatedEvent
  | InvoiceItemUpdatedEvent
  | InvoiceItemDeletedEvent
  | PlanCreatedEvent
  | PlanUpdatedEvent
  | PlanDeletedEvent
  | CouponCreatedEvent
  | CouponUpdatedEvent
  | CouponDeletedEvent
  | RecipientCreatedEvent
  | RecipientUpdatedEvent
  | RecipientDeletedEvent
  | TransferCreatedEvent
  | TransferUpdatedEvent
  | TransferCanceledEvent
  | TransferPaidEvent
  | TransferFailedEvent
  | PingEvent
  | UnknownEvent Text
  deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | Event Types JSON Instance
instance FromJSON EventType where
   parseJSON (String "account.updated") = pure AccountUpdatedEvent
   parseJSON (String "account.application.deauthorized") = pure AccountApplicationDeauthorizedEvent
   parseJSON (String "application_fee.created") = pure ApplicationFeeCreatedEvent
   parseJSON (String "application_fee.refunded") = pure ApplicationFeeRefundedEvent
   parseJSON (String "balance.available") = pure BalanceAvailableEvent
   parseJSON (String "charge.succeeded") = pure ChargeSucceededEvent
   parseJSON (String "charge.failed") = pure ChargeFailedEvent
   parseJSON (String "charge.refunded") = pure ChargeRefundedEvent
   parseJSON (String "charge.captured") = pure ChargeCapturedEvent
   parseJSON (String "charge.updated") = pure ChargeUpdatedEvent
   parseJSON (String "charge.dispute.created") = pure ChargeDisputeCreatedEvent
   parseJSON (String "charge.dispute.updated") = pure ChargeDisputeUpdatedEvent
   parseJSON (String "charge.dispute.closed") = pure ChargeDisputeClosedEvent
   parseJSON (String "charge.dispute.funds_withdrawn") = pure ChargeDisputeFundsWithdrawnEvent
   parseJSON (String "charge.dispute.funds_reinstated") = pure ChargeDisputeFundsReinstatedEvent
   parseJSON (String "customer.created") = pure CustomerCreatedEvent
   parseJSON (String "customer.updated") = pure CustomerUpdatedEvent
   parseJSON (String "customer.deleted") = pure CustomerDeletedEvent
   parseJSON (String "customer.card.created") = pure CustomerCardCreatedEvent
   parseJSON (String "customer.card.updated") = pure CustomerCardUpdatedEvent
   parseJSON (String "customer.card.deleted") = pure CustomerCardDeletedEvent
   parseJSON (String "customer.subscription.created") = pure CustomerSubscriptionCreatedEvent
   parseJSON (String "customer.subscription.updated") = pure CustomerSubscriptionUpdatedEvent
   parseJSON (String "customer.subscription.deleted") = pure CustomerSubscriptionDeletedEvent
   parseJSON (String "customer.subscription.trial_will_end") = pure CustomerSubscriptionTrialWillEndEvent
   parseJSON (String "customer.discount.created") = pure CustomerDiscountCreatedEvent
   parseJSON (String "customer.discount.updated") = pure CustomerDiscountUpdatedEvent
   parseJSON (String "invoice.created") = pure InvoiceCreatedEvent
   parseJSON (String "invoice.updated") = pure InvoiceUpdatedEvent
   parseJSON (String "invoice.payment_succeeded") = pure InvoicePaymentSucceededEvent
   parseJSON (String "invoice.payment_failed") = pure InvoicePaymentFailedEvent
   parseJSON (String "invoiceitem.created") = pure InvoiceItemCreatedEvent
   parseJSON (String "invoiceitem.updated") = pure InvoiceItemUpdatedEvent
   parseJSON (String "invoiceitem.deleted") = pure InvoiceItemDeletedEvent
   parseJSON (String "plan.created") = pure PlanCreatedEvent
   parseJSON (String "plan.updated") = pure PlanUpdatedEvent
   parseJSON (String "plan.deleted") = pure PlanDeletedEvent
   parseJSON (String "coupon.created") = pure CouponCreatedEvent
   parseJSON (String "coupon.updated") = pure CouponUpdatedEvent
   parseJSON (String "coupon.deleted") = pure CouponDeletedEvent
   parseJSON (String "recipient.created") = pure RecipientCreatedEvent
   parseJSON (String "recipient.updated") = pure RecipientUpdatedEvent
   parseJSON (String "recipient.deleted") = pure RecipientDeletedEvent
   parseJSON (String "transfer.created") = pure TransferCreatedEvent
   parseJSON (String "transfer.updated") = pure TransferUpdatedEvent
   parseJSON (String "transfer.canceled") = pure TransferCanceledEvent
   parseJSON (String "transfer.paid") = pure TransferPaidEvent
   parseJSON (String "transfer.failed") = pure TransferFailedEvent
   parseJSON (String "ping") = pure PingEvent
   parseJSON (String t) = pure $ UnknownEvent t
   parseJSON _ = mempty

------------------------------------------------------------------------------
-- | `EventId` of an `Event`
newtype EventId = EventId Text deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | EventData
data EventData =
    TransferEvent Transfer
  | AccountEvent Account
  | AccountApplicationEvent ConnectApp
  | ApplicationFeeEvent ApplicationFee
  | InvoiceEvent Invoice
  | PlanEvent Plan
  | RecipientEvent Recipient
  | CouponEvent Coupon
  | BalanceEvent Balance
  | ChargeEvent Charge
  | DisputeEvent Dispute
  | CustomerEvent Customer
  | CardEvent Card
  | SubscriptionEvent Subscription
  | DiscountEvent Discount
  | InvoiceItemEvent InvoiceItem
  | UnknownEventData
  | Ping
  deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | `Event` Object
data Event = Event {
      eventId              :: Maybe EventId
    , eventCreated         :: UTCTime
    , eventLiveMode        :: Bool
    , eventType            :: EventType
    , eventData            :: EventData
    , eventObject          :: Text
    , eventPendingWebHooks :: Int
    , eventRequest         :: Maybe Text
} deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `Event`
instance FromJSON Event where
   parseJSON (Object o) = do
     eventId <- fmap EventId <$> o .:? "id"
     eventCreated <- fromSeconds <$> o .: "created"
     eventLiveMode <- o .: "livemode"
     eventType <- o .: "type"
     String etype <- o .: "type"
     obj <- o .: "data"
     eventData <-
       case etype of
        "account.updated" -> AccountEvent <$> obj .: "object"
        "account.application.deauthorized" -> AccountApplicationEvent <$> obj .: "object"
        "application_fee.created" -> ApplicationFeeEvent <$> obj .: "object"
        "application_fee.refunded" -> ApplicationFeeEvent <$> obj .: "object"
        "balance.available" -> BalanceEvent <$> obj .: "object"
        "charge.succeeded" -> ChargeEvent <$> obj .: "object"
        "charge.failed" -> ChargeEvent <$> obj .: "object"
        "charge.refunded" -> ChargeEvent <$> obj .: "object"
        "charge.captured" -> ChargeEvent <$> obj .: "object"
        "charge.updated" -> ChargeEvent <$> obj .: "object"
        "charge.dispute.created" -> DisputeEvent <$> obj .: "object"
        "charge.dispute.updated" -> DisputeEvent <$> obj .: "object"
        "charge.dispute.closed" -> DisputeEvent <$> obj .: "object"
        "charge.dispute.funds_withdrawn" -> DisputeEvent <$> obj .: "object"
        "charge.dispute.funds_reinstated" -> DisputeEvent <$> obj .: "object"
        "customer.created" -> CustomerEvent <$> obj .: "object"
        "customer.updated" -> CustomerEvent <$> obj .: "object"
        "customer.deleted" -> CustomerEvent <$> obj .: "object"
        "customer.card.created" -> CardEvent <$> obj .: "object"
        "customer.card.updated" -> CardEvent <$> obj .: "object"
        "customer.card.deleted" -> CardEvent <$> obj .: "object"
        "customer.subscription.created" -> SubscriptionEvent <$> obj .: "object"
        "customer.subscription.updated" -> SubscriptionEvent <$> obj .: "object"
        "customer.subscription.deleted" -> SubscriptionEvent <$> obj .: "object"
        "customer.subscription.trial_will_end" -> SubscriptionEvent <$> obj .: "object"
        "customer.discount.created" -> DiscountEvent <$> obj .: "object"
        "customer.discount.updated" -> DiscountEvent <$> obj .: "object"
        "customer.discount.deleted" -> DiscountEvent <$> obj .: "object"
        "invoice.created" -> InvoiceEvent <$> obj .: "object"
        "invoice.updated" -> InvoiceEvent <$> obj .: "object"
        "invoice.payment_succeeded" -> InvoiceEvent <$> obj .: "object"
        "invoice.payment_failed" -> InvoiceEvent <$> obj .: "object"
        "invoiceitem.created" -> InvoiceItemEvent <$> obj .: "object"
        "invoiceitem.updated" -> InvoiceItemEvent <$> obj .: "object"
        "invoiceitem.deleted" -> InvoiceItemEvent <$> obj .: "object"
        "plan.created" -> PlanEvent <$> obj .: "object"
        "plan.updated" -> PlanEvent <$> obj .: "object"
        "plan.deleted" -> PlanEvent <$> obj .: "object"
        "coupon.created" -> CouponEvent <$> obj .: "object"
        "coupon.updated" -> CouponEvent <$> obj .: "object"
        "coupon.deleted" -> CouponEvent <$> obj .: "object"
        "recipient.created" -> RecipientEvent <$> obj .: "object"
        "recipient.updated" -> RecipientEvent <$> obj .: "object"
        "recipient.deleted" -> RecipientEvent <$> obj .: "object"
        "transfer.created" -> TransferEvent <$> obj .: "object"
        "transfer.updated" -> TransferEvent <$> obj .: "object"
        "transfer.canceled" -> TransferEvent <$> obj .: "object"
        "transfer.paid" -> TransferEvent <$> obj .: "object"
        "transfer.failed" -> TransferEvent <$> obj .: "object"
        "ping" -> pure Ping
        _        -> pure UnknownEventData
     eventObject <- o .: "object"
     eventPendingWebHooks <- o .: "pending_webhooks"
     eventRequest <- o .:? "request"
     return Event {..}
   parseJSON _ = mzero

------------------------------------------------------------------------------
-- | `PaymentIntentId` for `PaymentIntent`
newtype PaymentIntentId =
  PaymentIntentId { getPaymentIntentId :: Text } deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | `PaymentIntent` Object
data PaymentIntent = PaymentIntent {
      paymentIntentId                        :: PaymentIntentId
    , paymentIntentAmount                    :: Int
    , paymentIntentAmountCapturable          :: Maybe Int
    , paymentIntentAmountReceived            :: Maybe Int
    , paymentIntentApplication               :: Maybe (Expandable ApplicationId)
    , paymentIntentApplicationFeeAmount      :: Maybe Int
    , paymentIntentCanceledAt                :: Maybe UTCTime
    , paymentIntentCancellationReason        :: Maybe CancellationReason
    , paymentIntentCaptureMethod             :: CaptureMethod
    , paymentIntentCharges                   :: Maybe (StripeList Charge)
    , paymentIntentClientSecret              :: Maybe (Text)
    , paymentIntentConfirmationMethod        :: ConfirmationMethod
    , paymentIntentCreated                   :: UTCTime
    , paymentIntentCurrency                  :: Currency
    , paymentIntentCustomer                  :: Maybe (Expandable CustomerId)
    , paymentInventInvoice                   :: Maybe (Expandable InvoiceId)
    , paymentIntentLastPaymentError          :: Maybe TODO
    , paymentIntentLiveMode                  :: Maybe Bool
    , paymentIntentMetadata                  :: Maybe MetaData
    , paymentIntentNextAction                :: Maybe TODO
    , paymentIntentOnBehalfOf                :: Maybe (Expandable AccountId)
    , paymentIntentPaymentMethod             :: Maybe TODO
    , paymentIntentPaymentOptions            :: Maybe TODO
    , paymentIntentPaymentMethodTypes        :: [Text]
    , paymentIntentReceiptEmail              :: Maybe ReceiptEmail
    , paymentIntentReview                    :: Maybe TODO
    , paymentIntentSetupFutureUsage          :: Maybe Text
    , paymentIntentShipping                  :: Maybe TODO
    , paymentIntentStatementDescriptor       :: Maybe StatementDescription
    , paymentIntentStatementDescriptorSuffix :: Maybe StatementDescription
    , paymentIntentStatus                    :: PaymentIntentStatus
    , paymentIntentTransferData              :: Maybe TODO
    , paymentIntentTransferGroup             :: Maybe Text
    } deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `PaymentIntent`

instance FromJSON PaymentIntent where
  parseJSON = withObject "PaymentIntent" $ \o ->
    PaymentIntent
      <$> (PaymentIntentId <$> o .: "id")
      <*> o .: "amount"
      <*> o .:? "amount_capturable"
      <*> o .:? "amount_received"
      <*> o .:? "application"
      <*> o .:? "application_fee_amount"
      <*> o .:? "canceled_at"
      <*> o .:? "cancellation_reason"
      <*> o .: "capture_method"
      <*> o .:? "charges"
      <*> o .:? "client_secret"
      <*> o .: "confirmation_method"
      <*> o .: "created"
      <*> o .: "currency"
      <*> o .:? "customer"
      <*> o .:? "invoice"
      <*> o .:? "last_payment_error"
      <*> o .:? "live_mode"
      <*> o .:? "metadata"
      <*> o .:? "next_action"
      <*> o .:? "on_behalf_of"
      <*> o .:? "payment_method"
      <*> o .:? "payment_options"
      <*> o .: "payment_method_types"
      <*> (fmap ReceiptEmail <$> o .:? "receipt_email")
      <*> o .:? "review"
      <*> o .:? "setup_future_usage"
      <*> o .:? "shipping"
      <*> o .:? "statement_descriptor"
      <*> o .:? "statement_descriptor_suffix"
      <*> o .: "status"
      <*> o .:? "transfer_data"
      <*> o .:? "transfer_group"

data TODO = TODO
  deriving (Read, Show, Eq, Ord, Data, Typeable)

instance FromJSON TODO where
  parseJSON _ = pure TODO

data Application = Application {
    applicationId :: ApplicationId
  , applicationName :: Maybe Text
  } deriving (Read, Show, Eq, Ord, Data, Typeable)

instance FromJSON Application where
  parseJSON = withObject "Application" $ \o ->
    Application
      <$> ApplicationId <$> (o .: "id")
      <*> o .:? "name"

data CancellationReason
  = CancellationReasonDuplicate
  | CancellationReasonFraudulent
  | CancellationReasonRequestedByCustomer
  | CancellationReasonAbandoned
  | CancellationReasonFailedInvoice
  | CancellationReasonVoidInvoice
  | CancellationReasonAutomatic
  deriving (Read, Show, Eq, Ord, Data, Typeable)

instance FromJSON CancellationReason where
  parseJSON = withText "CancellationReason" $ \t -> case t of
    "duplicate" -> pure CancellationReasonDuplicate
    "fraudulent" -> pure CancellationReasonFraudulent
    "requestedByCustomer" -> pure CancellationReasonRequestedByCustomer
    "abandoned" -> pure CancellationReasonAbandoned
    "failedInvoice" -> pure CancellationReasonFailedInvoice
    "voidInvoice" -> pure CancellationReasonVoidInvoice
    "automatic" -> pure CancellationReasonAutomatic
    _ -> fail $ "unknown CancellationReason: " <> T.unpack t


data CaptureMethod
  = CaptureMethodAutomatic
  | CaptureMethodManual
  deriving (Read, Show, Eq, Ord, Data, Typeable)

instance FromJSON CaptureMethod where
  parseJSON = withText "CaptureMethod" $ \t -> case t of
    "automatic" -> pure CaptureMethodAutomatic
    "manual" -> pure CaptureMethodManual
    _ -> fail $ "Unknown CaptureMethod: " <> T.unpack t

data ConfirmationMethod
  = ConfirmationMethodAutomatic
  | ConfirmationMethodManual
  deriving (Read, Show, Eq, Ord, Data, Typeable)

instance FromJSON ConfirmationMethod where
  parseJSON = withText "ConfirmationMethod" $ \t -> case t of
    "automatic" -> pure ConfirmationMethodAutomatic
    "manual" -> pure ConfirmationMethodManual
    _ -> fail $ "Unknown ConfirmationMethod: " <> T.unpack t

data PaymentIntentStatus
  = PaymentIntentStatusCanceled
  | PaymentIntentStatusProcessing
  | PaymentIntentStatusRequiresAction
  | PaymentIntentStatusRequiresCapture
  | PaymentIntentStatusRequiresConfirmation
  | PaymentIntentStatusRequiresPaymentMethod
  | PaymentIntentStatusSucceeded
  deriving (Read, Show, Eq, Ord, Data, Typeable)

instance FromJSON PaymentIntentStatus where
  parseJSON = withText "PaymentIntentStatus" $ \t -> case t of
    "canceled" -> pure PaymentIntentStatusCanceled
    "processing" -> pure PaymentIntentStatusProcessing
    "requiresAction" -> pure PaymentIntentStatusRequiresAction
    "requiresCapture" -> pure PaymentIntentStatusRequiresCapture
    "requiresConfirmation" -> pure PaymentIntentStatusRequiresConfirmation
    "requiresPaymentMethod" -> pure PaymentIntentStatusRequiresPaymentMethod
    "succeeded" -> pure PaymentIntentStatusSucceeded
    _ -> fail $ "Unknown PaymentIntentStatus: " <> T.unpack t

newtype PaymentMethodId =
  PaymentMethodId Text deriving (Read, Show, Eq, Ord, Data, Typeable)


data PaymentMethod = PaymentMethod {
      paymentMethodId                        :: PaymentMethodId
    , paymentMethodBillingDetails            :: TODO
    , paymentMethodCard                      :: Maybe TODO
    , paymentMethodCardPresent               :: Maybe TODO
    , paymentMethodCreated                   :: UTCTime
    , paymentMethodCustomer                  :: Maybe (Expandable CustomerId)
    , paymentMethodLiveMode                  :: Bool
    , paymentMethodType                      :: PaymentMethodType
    } deriving (Read, Show, Eq, Ord, Data, Typeable)

data PaymentMethodType
  = PaymentMethodTypeCard
  | PaymentMethodTypeCardPresent
  deriving (Read, Show, Eq, Ord, Data, Typeable)

instance FromJSON PaymentMethodType where
  parseJSON = withText "PaymentMethodType" $ \t -> case t of
    "PaymentMethodTypeCard" -> pure PaymentMethodTypeCard
    "PaymentMethodTypeCardPresent" -> pure PaymentMethodTypeCardPresent
    _ -> fail $ "Unknown PaymentMethodType: " <> T.unpack t


------------------------------------------------------------------------------
-- | Connect Application
data ConnectApp = ConnectApp {
      connectAppId     :: Maybe Text
    , connectAppObject :: Text
    , connectAppName   :: Text
  } deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | Connect Application JSON instance
instance FromJSON ConnectApp where
   parseJSON (Object o) =
     ConnectApp <$> o .:? "id"
                <*> o .: "object"
                <*> o .: "name"
   parseJSON _  = mzero

------------------------------------------------------------------------------
-- | `TokenId` of a `Token`
newtype TokenId =
    TokenId Text
    deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | Type of `Token`
data TokenType = TokenCard
               | TokenBankAccount
                 deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `TokenType`
instance FromJSON TokenType where
   parseJSON (String "bank_account") = pure TokenBankAccount
   parseJSON (String "card") = pure TokenCard
   parseJSON _ = mzero

------------------------------------------------------------------------------
-- | `Token` Object
data Token a = Token {
      tokenId       :: TokenId
    , tokenLiveMode :: Bool
    , tokenCreated  :: UTCTime
    , tokenUsed     :: Bool
    , tokenObject   :: Text
    , tokenType     :: TokenType
    , tokenData     :: a
} deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `Token`
instance FromJSON a => FromJSON (Token a) where
   parseJSON (Object o) = do
     tokenId <- TokenId <$> o .: "id"
     Bool tokenLiveMode <- o .: "livemode"
     tokenCreated <- fromSeconds <$> o .: "created"
     Bool tokenUsed <- o .: "used"
     String tokenObject <- o .: "object"
     String typ <- o .: "type"
     tokenType <- pure $ case typ of
                      "card"         -> TokenCard
                      "bank_account" -> TokenBankAccount
                      _ -> error "unspecified type"
     tokenData <-
       case typ of
        "bank_account" -> o .: "bank_account"
        "card"         -> o .: "card"
        _              -> mzero
     return Token {..}
   parseJSON _ = mzero

------------------------------------------------------------------------------
-- | Generic handling of Stripe JSON arrays
data StripeList a = StripeList {
      list       :: [a]
    , stripeUrl  :: Text
    , object     :: Text
    , totalCount :: Maybe Int
    , hasMore    :: Bool
    } deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `StripeList`
instance FromJSON a => FromJSON (StripeList a) where
    parseJSON (Object o) =
        StripeList <$> o .:  "data"
                   <*> o .:  "url"
                   <*> o .:  "object"
                   <*> o .:? "total_count"
                   <*> o .:  "has_more"
    parseJSON _ = mzero

------------------------------------------------------------------------------
-- | Pagination Option for `StripeList`
newtype Limit = Limit Int
  deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | Pagination Option for `StripeList`
newtype StartingAfter a = StartingAfter a
 deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | Pagination Option for `StripeList`
newtype EndingBefore a = EndingBefore a
 deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON returned from a `Stripe` deletion request
data StripeDeleteResult = StripeDeleteResult {
      deleted   :: Bool
    , deletedId :: Maybe Text
    } deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `StripeDeleteResult`
instance FromJSON StripeDeleteResult where
   parseJSON (Object o) =
       StripeDeleteResult <$> o .: "deleted"
                          <*> o .:? "id"
   parseJSON _ = mzero

------------------------------------------------------------------------------
-- | Type of MetaData for use on `Stripe` objects
newtype MetaData = MetaData [ (Text,Text) ]
  deriving (Read, Show, Eq, Ord, Data, Typeable)

instance FromJSON MetaData where
  parseJSON j = (MetaData . H.toList) <$> (parseJSON j)

------------------------------------------------------------------------------
-- | Type of Expansion Parameters for use on `Stripe` objects
newtype ExpandParams = ExpandParams { getExpandParams :: [Text] }
  deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | Generic ID for use in constructing API Calls
type ID    = Text

------------------------------------------------------------------------------
-- | Generic URL for use in constructing API Calls
type URL   = Text

------------------------------------------------------------------------------
-- | a cardholder's full name
newtype Name  = Name { getName :: Text }
   deriving (Read, Show, Eq, Ord, Data, Typeable)

instance FromJSON Name where
  parseJSON v = Name <$> parseJSON v

------------------------------------------------------------------------------
-- | a plan name
newtype PlanName  = PlanName { getPlanName :: Text }
   deriving (Read, Show, Eq, Ord, Data, Typeable)

instance FromJSON PlanName where
  parseJSON v = PlanName <$> parseJSON v

------------------------------------------------------------------------------
-- | Generic Description for use in constructing API Calls
newtype Description = Description Text
   deriving (Read, Show, Eq, Ord, Data, Typeable)

instance FromJSON Description where
  parseJSON v = Description <$> parseJSON v

------------------------------------------------------------------------------
-- | Generic `Quantity` type to be used with `Customer`,
-- `Subscription` and `InvoiceLineItem` API requests
newtype Quantity = Quantity Int deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | Prorate
newtype Prorate = Prorate Bool deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | A flag that if set to true will delay the cancellation of the
-- subscription until the end of the current period.
newtype AtPeriodEnd = AtPeriodEnd Bool deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | `Email` associated with a `Customer`, `Recipient` or `Charge`
newtype Email = Email Text deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | `Email` to send receipt to
newtype ReceiptEmail = ReceiptEmail Text deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | Stripe supports 138 currencies
data Currency =
    AED -- ^  United Arab Emirates Dirham
  | AFN -- ^  Afghan Afghani
  | ALL -- ^  Albanian Lek
  | AMD -- ^  Armenian Dram
  | ANG -- ^  Netherlands Antillean Gulden
  | AOA -- ^  Angolan Kwanza
  | ARS -- ^  Argentine Peso
  | AUD -- ^  Australian Dollar
  | AWG -- ^  Aruban Florin
  | AZN -- ^  Azerbaijani Manat
  | BAM -- ^  Bosnia & Herzegovina Convertible Mark
  | BBD -- ^  Barbadian Dollar
  | BDT -- ^  Bangladeshi Taka
  | BGN -- ^  Bulgarian Lev
  | BIF -- ^  Burundian Franc
  | BMD -- ^  Bermudian Dollar
  | BND -- ^  Brunei Dollar
  | BOB -- ^  Bolivian Boliviano
  | BRL -- ^  Brazilian Real
  | BSD -- ^  Bahamian Dollar
  | BWP -- ^  Botswana Pula
  | BZD -- ^  Belize Dollar
  | CAD -- ^  Canadian Dollar
  | CDF -- ^  Congolese Franc
  | CHF -- ^  Swiss Franc
  | CLP -- ^  Chilean Peso
  | CNY -- ^  Chinese Renminbi Yuan
  | COP -- ^  Colombian Peso
  | CRC -- ^  Costa Rican Colón
  | CVE -- ^  Cape Verdean Escudo
  | CZK -- ^  Czech Koruna
  | DJF -- ^  Djiboutian Franc
  | DKK -- ^  Danish Krone
  | DOP -- ^  Dominican Peso
  | DZD -- ^  Algerian Dinar
  | EEK -- ^  Estonian Kroon
  | EGP -- ^  Egyptian Pound
  | ETB -- ^  Ethiopian Birr
  | EUR -- ^  Euro
  | FJD -- ^  Fijian Dollar
  | FKP -- ^  Falkland Islands Pound
  | GBP -- ^  British Pound
  | GEL -- ^  Georgian Lari
  | GIP -- ^  Gibraltar Pound
  | GMD -- ^  Gambian Dalasi
  | GNF -- ^  Guinean Franc
  | GTQ -- ^  Guatemalan Quetzal
  | GYD -- ^  Guyanese Dollar
  | HKD -- ^  Hong Kong Dollar
  | HNL -- ^  Honduran Lempira
  | HRK -- ^  Croatian Kuna
  | HTG -- ^  Haitian Gourde
  | HUF -- ^  Hungarian Forint
  | IDR -- ^  Indonesian Rupiah
  | ILS -- ^  Israeli New Sheqel
  | INR -- ^  Indian Rupee
  | ISK -- ^  Icelandic Króna
  | JMD -- ^  Jamaican Dollar
  | JPY -- ^  Japanese Yen
  | KES -- ^  Kenyan Shilling
  | KGS -- ^  Kyrgyzstani Som
  | KHR -- ^  Cambodian Riel
  | KMF -- ^  Comorian Franc
  | KRW -- ^  South Korean Won
  | KYD -- ^  Cayman Islands Dollar
  | KZT -- ^  Kazakhstani Tenge
  | LAK -- ^  Lao Kip
  | LBP -- ^  Lebanese Pound
  | LKR -- ^  Sri Lankan Rupee
  | LRD -- ^  Liberian Dollar
  | LSL -- ^  Lesotho Loti
  | LTL -- ^  Lithuanian Litas
  | LVL -- ^  Latvian Lats
  | MAD -- ^  Moroccan Dirham
  | MDL -- ^  Moldovan Leu
  | MGA -- ^  Malagasy Ariary
  | MKD -- ^  Macedonian Denar
  | MNT -- ^  Mongolian Tögrög
  | MOP -- ^  Macanese Pataca
  | MRO -- ^  Mauritanian Ouguiya
  | MUR -- ^  Mauritian Rupee
  | MVR -- ^  Maldivian Rufiyaa
  | MWK -- ^  Malawian Kwacha
  | MXN -- ^  Mexican Peso
  | MYR -- ^  Malaysian Ringgit
  | MZN -- ^  Mozambican Metical
  | NAD -- ^  Namibian Dollar
  | NGN -- ^  Nigerian Naira
  | NIO -- ^  Nicaraguan Córdoba
  | NOK -- ^  Norwegian Krone
  | NPR -- ^  Nepalese Rupee
  | NZD -- ^  New Zealand Dollar
  | PAB -- ^  Panamanian Balboa
  | PEN -- ^  Peruvian Nuevo Sol
  | PGK -- ^  Papua New Guinean Kina
  | PHP -- ^  Philippine Peso
  | PKR -- ^  Pakistani Rupee
  | PLN -- ^  Polish Złoty
  | PYG -- ^  Paraguayan Guaraní
  | QAR -- ^  Qatari Riyal
  | RON -- ^  Romanian Leu
  | RSD -- ^  Serbian Dinar
  | RUB -- ^  Russian Ruble
  | RWF -- ^  Rwandan Franc
  | SAR -- ^  Saudi Riyal
  | SBD -- ^  Solomon Islands Dollar
  | SCR -- ^  Seychellois Rupee
  | SEK -- ^  Swedish Krona
  | SGD -- ^  Singapore Dollar
  | SHP -- ^  Saint Helenian Pound
  | SLL -- ^  Sierra Leonean Leone
  | SOS -- ^  Somali Shilling
  | SRD -- ^  Surinamese Dollar
  | STD -- ^  São Tomé and Príncipe Dobra
  | SVC -- ^  Salvadoran Colón
  | SZL -- ^  Swazi Lilangeni
  | THB -- ^  Thai Baht
  | TJS -- ^  Tajikistani Somoni
  | TOP -- ^  Tongan Paʻanga
  | TRY -- ^  Turkish Lira
  | TTD -- ^  Trinidad and Tobago Dollar
  | TWD -- ^  New Taiwan Dollar
  | TZS -- ^  Tanzanian Shilling
  | UAH -- ^  Ukrainian Hryvnia
  | UGX -- ^  Ugandan Shilling
  | USD -- ^  United States Dollar
  | UYU -- ^  Uruguayan Peso
  | UZS -- ^  Uzbekistani Som
  | VND -- ^  Vietnamese Đồng
  | VUV -- ^  Vanuatu Vatu
  | WST -- ^  Samoan Tala
  | XAF -- ^  Central African Cfa Franc
  | XCD -- ^  East Caribbean Dollar
  | XOF -- ^  West African Cfa Franc
  | XPF -- ^  Cfp Franc
  | YER -- ^  Yemeni Rial
  | ZAR -- ^  South African Rand
  | ZMW -- ^  Zambian Kwacha
  | UnknownCurrency -- ^  Unknown Currency
    deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | `Currency` JSON instances
instance FromJSON Currency where
   parseJSON (String "aed") = pure AED
   parseJSON (String "afn") = pure AFN
   parseJSON (String "all") = pure ALL
   parseJSON (String "amd") = pure AMD
   parseJSON (String "ang") = pure ANG
   parseJSON (String "aoa") = pure AOA
   parseJSON (String "ars") = pure ARS
   parseJSON (String "aud") = pure AUD
   parseJSON (String "awg") = pure AWG
   parseJSON (String "azn") = pure AZN
   parseJSON (String "bam") = pure BAM
   parseJSON (String "bbd") = pure BBD
   parseJSON (String "bdt") = pure BDT
   parseJSON (String "bgn") = pure BGN
   parseJSON (String "bif") = pure BIF
   parseJSON (String "bmd") = pure BMD
   parseJSON (String "bnd") = pure BND
   parseJSON (String "bob") = pure BOB
   parseJSON (String "brl") = pure BRL
   parseJSON (String "bsd") = pure BSD
   parseJSON (String "bwp") = pure BWP
   parseJSON (String "bzd") = pure BZD
   parseJSON (String "cad") = pure CAD
   parseJSON (String "cdf") = pure CDF
   parseJSON (String "chf") = pure CHF
   parseJSON (String "clp") = pure CLP
   parseJSON (String "cny") = pure CNY
   parseJSON (String "cop") = pure COP
   parseJSON (String "crc") = pure CRC
   parseJSON (String "cve") = pure CVE
   parseJSON (String "czk") = pure CZK
   parseJSON (String "djf") = pure DJF
   parseJSON (String "dkk") = pure DKK
   parseJSON (String "dop") = pure DOP
   parseJSON (String "dzd") = pure DZD
   parseJSON (String "eek") = pure EEK
   parseJSON (String "egp") = pure EGP
   parseJSON (String "etb") = pure ETB
   parseJSON (String "eur") = pure EUR
   parseJSON (String "fjd") = pure FJD
   parseJSON (String "fkp") = pure FKP
   parseJSON (String "gbp") = pure GBP
   parseJSON (String "gel") = pure GEL
   parseJSON (String "gip") = pure GIP
   parseJSON (String "gmd") = pure GMD
   parseJSON (String "gnf") = pure GNF
   parseJSON (String "gtq") = pure GTQ
   parseJSON (String "gyd") = pure GYD
   parseJSON (String "hkd") = pure HKD
   parseJSON (String "hnl") = pure HNL
   parseJSON (String "hrk") = pure HRK
   parseJSON (String "htg") = pure HTG
   parseJSON (String "huf") = pure HUF
   parseJSON (String "idr") = pure IDR
   parseJSON (String "ils") = pure ILS
   parseJSON (String "inr") = pure INR
   parseJSON (String "isk") = pure ISK
   parseJSON (String "jmd") = pure JMD
   parseJSON (String "jpy") = pure JPY
   parseJSON (String "kes") = pure KES
   parseJSON (String "kgs") = pure KGS
   parseJSON (String "khr") = pure KHR
   parseJSON (String "kmf") = pure KMF
   parseJSON (String "krw") = pure KRW
   parseJSON (String "kyd") = pure KYD
   parseJSON (String "kzt") = pure KZT
   parseJSON (String "lak") = pure LAK
   parseJSON (String "lbp") = pure LBP
   parseJSON (String "lkr") = pure LKR
   parseJSON (String "lrd") = pure LRD
   parseJSON (String "lsl") = pure LSL
   parseJSON (String "ltl") = pure LTL
   parseJSON (String "lvl") = pure LVL
   parseJSON (String "mad") = pure MAD
   parseJSON (String "mdl") = pure MDL
   parseJSON (String "mga") = pure MGA
   parseJSON (String "mkd") = pure MKD
   parseJSON (String "mnt") = pure MNT
   parseJSON (String "mop") = pure MOP
   parseJSON (String "mro") = pure MRO
   parseJSON (String "mur") = pure MUR
   parseJSON (String "mvr") = pure MVR
   parseJSON (String "mwk") = pure MWK
   parseJSON (String "mxn") = pure MXN
   parseJSON (String "myr") = pure MYR
   parseJSON (String "mzn") = pure MZN
   parseJSON (String "nad") = pure NAD
   parseJSON (String "ngn") = pure NGN
   parseJSON (String "nio") = pure NIO
   parseJSON (String "nok") = pure NOK
   parseJSON (String "npr") = pure NPR
   parseJSON (String "nzd") = pure NZD
   parseJSON (String "pab") = pure PAB
   parseJSON (String "pen") = pure PEN
   parseJSON (String "pgk") = pure PGK
   parseJSON (String "php") = pure PHP
   parseJSON (String "pkr") = pure PKR
   parseJSON (String "pln") = pure PLN
   parseJSON (String "pyg") = pure PYG
   parseJSON (String "qar") = pure QAR
   parseJSON (String "ron") = pure RON
   parseJSON (String "rsd") = pure RSD
   parseJSON (String "rub") = pure RUB
   parseJSON (String "rwf") = pure RWF
   parseJSON (String "sar") = pure SAR
   parseJSON (String "sbd") = pure SBD
   parseJSON (String "scr") = pure SCR
   parseJSON (String "sek") = pure SEK
   parseJSON (String "sgd") = pure SGD
   parseJSON (String "shp") = pure SHP
   parseJSON (String "sll") = pure SLL
   parseJSON (String "sos") = pure SOS
   parseJSON (String "srd") = pure SRD
   parseJSON (String "std") = pure STD
   parseJSON (String "svc") = pure SVC
   parseJSON (String "szl") = pure SZL
   parseJSON (String "thb") = pure THB
   parseJSON (String "tjs") = pure TJS
   parseJSON (String "top") = pure TOP
   parseJSON (String "try") = pure TRY
   parseJSON (String "ttd") = pure TTD
   parseJSON (String "twd") = pure TWD
   parseJSON (String "tzs") = pure TZS
   parseJSON (String "uah") = pure UAH
   parseJSON (String "ugx") = pure UGX
   parseJSON (String "usd") = pure USD
   parseJSON (String "uyu") = pure UYU
   parseJSON (String "uzs") = pure UZS
   parseJSON (String "vnd") = pure VND
   parseJSON (String "vuv") = pure VUV
   parseJSON (String "wst") = pure WST
   parseJSON (String "xaf") = pure XAF
   parseJSON (String "xcd") = pure XCD
   parseJSON (String "xof") = pure XOF
   parseJSON (String "xpf") = pure XPF
   parseJSON (String "yer") = pure YER
   parseJSON (String "zar") = pure ZAR
   parseJSON (String "zmw") = pure ZMW
   parseJSON _ = pure UnknownCurrency

------------------------------------------------------------------------------
-- | BTC ReceiverObject
data BitcoinReceiver = BitcoinReceiver {
       btcId                    :: BitcoinReceiverId
    ,  btcObject                :: Text
    ,  btcCreated               :: UTCTime
    ,  btcLiveMode              :: Bool
    ,  btcActive                :: Bool
    ,  btcAmount                :: Integer
    ,  btcAmountReceived        :: Integer
    ,  btcBitcoinAmount         :: Integer
    ,  btcBitcoinAmountReceived :: Integer
    ,  btcBitcoinUri            :: Text
    ,  btcCurrency              :: Currency
    ,  btcFilled                :: Bool
    ,  btcInboundAddress        :: Text
    ,  btcUncapturedFunds       :: Bool
    ,  btcDescription           :: Maybe Text
    ,  btcEmail                 :: Text
    ,  btcMetadata              :: MetaData
    ,  btcRefundAddress         :: Maybe Text
    ,  btcTransactions          :: Maybe Transactions
    ,  btcPayment               :: Maybe PaymentId
    ,  btcCustomer              :: Maybe CustomerId
    } deriving (Show, Eq)

------------------------------------------------------------------------------
-- | FromJSON for BitcoinReceiverId
instance FromJSON BitcoinReceiver where
   parseJSON (Object o) =
     BitcoinReceiver <$> (BitcoinReceiverId <$> o .: "id")
                     <*> o .: "object"
                     <*> (fromSeconds <$> o .: "created")
                     <*> o .: "livemode"
                     <*> o .: "active"
                     <*> o .: "amount"
                     <*> o .: "amount_received"
                     <*> o .: "bitcoin_amount"
                     <*> o .: "bitcoin_amount_received"
                     <*> o .: "bitcoin_uri"
                     <*> o .: "currency"
                     <*> o .: "filled"
                     <*> o .: "inbound_address"
                     <*> o .: "uncaptured_funds"
                     <*> o .:? "description"
                     <*> o .: "email"
                     <*> (MetaData . H.toList <$> o .: "metadata")
                     <*> o .:? "refund_address"
                     <*> o .:? "transactions"
                     <*> (fmap PaymentId <$> o .:? "payment")
                     <*> (fmap CustomerId <$> o .:? "customer")
   parseJSON _ = mzero

------------------------------------------------------------------------------
-- | Bitcoin Transactions
data Transactions = Transactions {
      transactionsObject     :: Text
    , transactionsTotalCount :: Integer
    , transactionsHasMore    :: Bool
    , transactionsURL        :: Text
    , transactions           :: [BitcoinTransaction]
    } deriving (Show, Eq)

------------------------------------------------------------------------------
-- | Bitcoin Transactions data
instance FromJSON Transactions where
   parseJSON (Object o) =
     Transactions <$> o .: "object"
                  <*> o .: "total_count"
                  <*> o .: "has_more"
                  <*> o .: "url"
                  <*> o .: "data"
   parseJSON _ = mzero

------------------------------------------------------------------------------
-- | Bitcoin Transaction
data BitcoinTransaction = BitcoinTransaction {
         btcTransactionId            :: BitcoinTransactionId
       , btcTransactionObject        :: Text
       , btcTransactionCreated       :: UTCTime
       , btcTransactionAmount        :: Integer
       , btcTransactionBitcoinAmount :: Integer
       , btcTransactionCurrency      :: Currency
       , btcTransactionReceiver      :: BitcoinReceiverId
      } deriving (Show, Eq)

------------------------------------------------------------------------------
-- | FromJSON BitcoinTransaction
instance FromJSON BitcoinTransaction where
   parseJSON (Object o) =
     BitcoinTransaction <$> o .: "id"
                        <*> o .: "object"
                        <*> (fromSeconds <$> o .: "created")
                        <*> o .: "amount"
                        <*> o .: "bitcoin_amount"
                        <*> o .: "currency"
                        <*> o .: "receiver"
   parseJSON _ = mzero

------------------------------------------------------------------------------
-- | BitcoinTransactionId
newtype BitcoinTransactionId =
    BitcoinTransactionId Text
      deriving (Show, Eq)

------------------------------------------------------------------------------
-- | FromJSON BitcoinTransactionId
instance FromJSON BitcoinTransactionId where
   parseJSON (String o) = pure $ BitcoinTransactionId o
   parseJSON _ = mzero

------------------------------------------------------------------------------
-- | BTC ReceiverId
newtype BitcoinReceiverId = BitcoinReceiverId Text
    deriving (Show, Eq)

------------------------------------------------------------------------------
-- | FromJSON for BitcoinReceiverId
instance FromJSON BitcoinReceiverId where
   parseJSON (String x) = pure $ BitcoinReceiverId x
   parseJSON _ = mzero

------------------------------------------------------------------------------
-- | BTC PaymentId
newtype PaymentId = PaymentId Text
    deriving (Show, Eq)

------------------------------------------------------------------------------
-- | FromJSON for PaymentId
instance FromJSON PaymentId where
   parseJSON (String x) = pure $ PaymentId x
   parseJSON _ = mzero

------------------------------------------------------------------------------
-- | Show an amount accounting for zero currencies
--
-- https:\/\/support.stripe.com\/questions\/which-zero-decimal-currencies-does-stripe-support
showAmount
  :: Currency -- ^ `Currency`
  -> Int      -- ^ `Amount`
  -> String
showAmount cur amt =
  case cur of
   USD -> "$" ++ show2places (currencyDivisor cur amt)
   _   -> show2places (currencyDivisor cur amt) ++ " " ++ show cur
  where
    show2places v = showFFloat (Just 2) v ""

------------------------------------------------------------------------------
-- currency division funtion accounting for zero currencies
--
-- https:\/\/support.stripe.com\/questions\/which-zero-decimal-currencies-does-stripe-support
currencyDivisor
    :: Currency -- ^ `Currency`
    -> (Int -> Float) -- ^ function to convert amount to a float
currencyDivisor cur =
  case cur of
    BIF -> zeroCurrency
    CLP -> zeroCurrency
    DJF -> zeroCurrency
    GNF -> zeroCurrency
    JPY -> zeroCurrency
    KMF -> zeroCurrency
    KRW -> zeroCurrency
    MGA -> zeroCurrency
    PYG -> zeroCurrency
    RWF -> zeroCurrency
    VND -> zeroCurrency
    VUV -> zeroCurrency
    XAF -> zeroCurrency
    XOF -> zeroCurrency
    XPF -> zeroCurrency
    EUR -> hundred
    USD -> hundred
    CHF -> hundred
    _   -> error $ "please submit a patch to currencyDivisor for this currency: " ++ show cur
  where
    zeroCurrency = fromIntegral
    hundred v    = fromRat $ (fromIntegral v) % (100 :: Integer)
