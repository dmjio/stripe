{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
-- |
-- Module      : Web.Stripe.Types
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Web.Stripe.Types where

import           Control.Applicative        (pure, (<$>), (<*>), (<|>))
import           Control.Monad              (mzero)
import           Data.Aeson                 (FromJSON (parseJSON),
                                             Value (String, Object, Bool), (.:),
                                             (.:?))
import qualified Data.HashMap.Strict        as H
import           Data.Text                  (Text)
import           Data.Time                  (UTCTime)
import           Web.Stripe.Client.Internal (fromSeconds)

------------------------------------------------------------------------------
-- | `ChargeId` associated with a `Charge`
data ChargeId
  = ChargeId Text
  | ExpandedCharge Charge
  deriving (Show, Eq)

------------------------------------------------------------------------------
-- | JSON Instance for `ChargeId`
instance FromJSON ChargeId where
   parseJSON (String x)   = pure $ ChargeId x
   parseJSON o@(Object _) = ExpandedCharge <$> parseJSON o
   parseJSON _ = mzero

------------------------------------------------------------------------------
-- | `StatementDescription` to be added to a `Charge`
newtype StatementDescription =
  StatementDescription Text deriving (Show, Eq)

------------------------------------------------------------------------------
-- | `Charge` object in `Stripe` API
data Charge = Charge {
      chargeId                   :: ChargeId
    , chargeObject               :: Text
    , chargeCreated              :: UTCTime
    , chargeLiveMode             :: Bool
    , chargePaid                 :: Bool
    , chargeAmount               :: Int
    , chargeCurrency             :: Currency
    , chargeRefunded             :: Bool
    , chargeCreditCard           :: Card
    , chargeCaptured             :: Bool
    , chargeRefunds              :: StripeList Refund
    , chargeBalanceTransaction   :: Maybe TransactionId
    , chargeFailureMessage       :: Maybe Text
    , chargeFailureCode          :: Maybe Text
    , chargeAmountRefunded       :: Int
    , chargeCustomerId           :: Maybe CustomerId
    , chargeInvoice              :: Maybe InvoiceId
    , chargeDescription          :: Maybe Text
    , chargeDispute              :: Maybe Text
    , chargeMetaData             :: MetaData
    , chargeStatementDescription :: Maybe Text
    , chargeReceiptEmail         :: Maybe Text
    , chargeReceiptNumber        :: Maybe Text
    } deriving (Show, Eq)

------------------------------------------------------------------------------
-- | JSON Instance for `Charge`
instance FromJSON Charge where
    parseJSON (Object o) =
        Charge <$> (ChargeId <$> o .: "id")
               <*> o .: "object"
               <*> (fromSeconds <$> o .: "created")
               <*> o .: "livemode"
               <*> o .: "paid"
               <*> o .: "amount"
               <*> (Currency <$> o .: "currency")
               <*> o .: "refunded"
               <*> o .: "card"
               <*> o .: "captured"
               <*> o .: "refunds"
               <*> ((fmap TransactionId <$> o .:? "balance_transaction")
               <|> (fmap ExpandedTransaction <$> o .:? "balance_transaction"))
               <*> o .:? "failure_message"
               <*> o .:? "failure_code"
               <*> o .: "amount_refunded"
               <*> ((fmap CustomerId <$> o .:? "customer")
               <|> (fmap ExpandedCustomer <$> o .:? "customer"))
               <*> ((fmap InvoiceId <$> o .:? "invoice")
               <|> (fmap ExpandedInvoice <$> o .:? "invoice"))
               <*> o .:? "description"
               <*> o .:? "dispute"
               <*> (H.toList <$> o .: "metadata")
               <*> o .:? "statement_description"
               <*> o .:? "receipt_email"
               <*> o .:? "receipt_number"
    parseJSON _ = mzero

------------------------------------------------------------------------------
-- | Capture for `Charge`
type Capture = Bool

------------------------------------------------------------------------------
-- | Refund

------------------------------------------------------------------------------
-- | `RefundId` for `Refund`
newtype RefundId =
  RefundId Text deriving (Eq, Show)

------------------------------------------------------------------------------
-- | `Refund` Object
data Refund = Refund {
      refundId                 :: RefundId
    , refundAmount             :: Int
    , refundCurrency           :: Text
    , refundCreated            :: UTCTime
    , refundObject             :: Text
    , refundCharge             :: ChargeId
    , refundBalanceTransaction :: TransactionId
    , refundMetaData           :: MetaData
    } deriving (Show, Eq)

------------------------------------------------------------------------------
-- | JSON Instance for `Refund`
instance FromJSON Refund where
   parseJSON (Object o) =
        Refund <$> (RefundId <$> o .: "id")
               <*> o .: "amount"
               <*> o .: "currency"
               <*> (fromSeconds <$> o .: "created")
               <*> o .: "object"
               <*> (ChargeId <$> o .: "charge")
               <*> ((TransactionId <$> o .: "balance_transaction")
               <|> (ExpandedTransaction <$> o .: "balance_transaction"))
               <*> (H.toList <$> o .: "metadata")
   parseJSON _ = mzero

------------------------------------------------------------------------------
-- | Customer

------------------------------------------------------------------------------
-- | `CustomerId` for a `Customer`
data CustomerId =
  CustomerId Text |
  ExpandedCustomer Customer
  deriving (Show, Eq)

------------------------------------------------------------------------------
-- | JSON Instance for `CustomerId`
instance FromJSON CustomerId where
    parseJSON (String x)   = pure (CustomerId x)
    parseJSON v@(Object _) = ExpandedCustomer <$> parseJSON v
    parseJSON _            = mzero

------------------------------------------------------------------------------
-- | `Customer` object
data Customer = Customer {
      customerObject         :: Text
    , customerCreated        :: UTCTime
    , customerId             :: CustomerId
    , customerLiveMode       :: Bool
    , customerDescription    :: Maybe Text
    , customerEmail          :: Maybe Email
    , customerDelinquent     :: Bool
    , customerSubscriptions  :: StripeList Subscription
    , customerDiscount       :: Maybe Discount
    , customerAccountBalance :: Int
    , customerCards          :: StripeList Card
    , customerCurrency       :: Maybe Currency
    , customerDefaultCard    :: Maybe CardId
    , customerMetaData       :: MetaData
    } | DeletedCustomer {
      deletedCustomer   :: Bool
    , deletedCustomerId :: CustomerId
  } deriving (Show, Eq)

------------------------------------------------------------------------------
-- | JSON Instance for `Customer`
instance FromJSON Customer where
    parseJSON (Object o)
        = (Customer
           <$> o .: "object"
           <*> (fromSeconds <$> o .: "created")
           <*> (CustomerId <$> o .: "id")
           <*> o .: "livemode"
           <*> o .:? "description"
           <*> (fmap Email <$> o .:? "email")
           <*> o .: "delinquent"
           <*> o .: "subscriptions"
           <*> o .:? "discount"
           <*> o .: "account_balance"
           <*> o .: "cards"
           <*> (fmap Currency <$> o .:? "currency")
           <*> ((fmap CardId <$> o .:? "default_card")
           <|> (fmap ExpandedCard <$> o.:? "default_card"))
           <*> (H.toList <$> o .: "metadata")
           <|> DeletedCustomer
           <$> o .: "deleted"
           <*> (CustomerId <$> o .: "id"))
           <|> DeletedCustomer
           <$> o .: "deleted"
           <*> (CustomerId <$> o .: "id")
    parseJSON _ = mzero

------------------------------------------------------------------------------
-- | AccountBalance for a `Customer`
type AccountBalance = Int

------------------------------------------------------------------------------
-- | Card

------------------------------------------------------------------------------
-- | CardId for a `Customer`
data CardId = CardId Text
            | ExpandedCard Card
            deriving (Eq, Show)

------------------------------------------------------------------------------
-- | CardId for a `Recipient`
data RecipientCardId = RecipientCardId Text
                     | ExpandedRecipientCard RecipientCard
                     deriving (Eq, Show)

------------------------------------------------------------------------------
-- | JSON Instance for `CardId`
instance FromJSON CardId where
   parseJSON o@(Object _) = ExpandedCard <$> parseJSON o
   parseJSON (String x)   = pure $ CardId x
   parseJSON _ = mzero

------------------------------------------------------------------------------
-- | CardNumber associated with a Card
newtype CardNumber     = CardNumber Text deriving (Show, Eq)

------------------------------------------------------------------------------
-- | Expiration Month for a `Card`
newtype ExpMonth       = ExpMonth Int deriving (Show, Eq)

------------------------------------------------------------------------------
-- | Expiration Year for a `Card`
newtype ExpYear        = ExpYear Int deriving (Show, Eq)

------------------------------------------------------------------------------
-- | CVC for a `Card`
newtype CVC            = CVC Text deriving (Show, Eq)

------------------------------------------------------------------------------
-- | City address for a `Card`
newtype AddressCity    = AddressCity Text deriving (Show, Eq)

------------------------------------------------------------------------------
-- | Country address for a `Card`
newtype AddressCountry = AddressCountry Text deriving (Show, Eq)

------------------------------------------------------------------------------
-- | Address Line One for a `Card`
newtype AddressLine1   = AddressLine1 Text deriving (Show, Eq)

------------------------------------------------------------------------------
-- | Address Line Two for a `Card`
newtype AddressLine2   = AddressLine2 Text deriving (Show, Eq)

------------------------------------------------------------------------------
-- | Address State for a `Card`
newtype AddressState   = AddressState Text deriving (Show, Eq)

------------------------------------------------------------------------------
-- | Address Zip Code for a `Card`
newtype AddressZip     = AddressZip Text deriving (Show, Eq)

------------------------------------------------------------------------------
-- | Credit Card Brand
data Brand = Visa
           | AMEX
           | MasterCard
           | Discover
           | JCB
           | DinersClub
           | Unknown
             deriving (Show, Eq)

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
    , cardFingerprint       :: Text
    , cardCountry           :: Text
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
    , cardCustomerId        :: Maybe CustomerId
    } deriving (Show, Eq)

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
    , recipientCardRecipientId       :: Maybe RecipientId
} deriving (Show, Eq)

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
             <*> o .: "fingerprint"
             <*> o .: "country"
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
             <*> ((fmap CustomerId <$> o .:? "customer")
             <|> (fmap ExpandedCustomer <$> o .:? "customer"))
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
             <*> ((fmap RecipientId <$> o .:? "recipient")
             <|> (fmap ExpandedRecipient <$> o .:? "recipient"))
    parseJSON _ = mzero

------------------------------------------------------------------------------
-- | Subscription

------------------------------------------------------------------------------
-- | `SubscriptionId` for a `Subscription`
newtype SubscriptionId =
    SubscriptionId Text
    deriving (Show, Eq)

------------------------------------------------------------------------------
-- | Subscription Object
data Subscription = Subscription {
      subscriptionId                    :: SubscriptionId
    , subscriptionPlan                  :: Plan
    , subscriptionObject                :: Text
    , subscriptionStart                 :: UTCTime
    , subscriptionStatus                :: SubscriptionStatus
    , subscriptionCustomerId            :: CustomerId
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

} deriving (Show, Eq)

------------------------------------------------------------------------------
-- | JSON Instance for `Subscription`
instance FromJSON Subscription where
   parseJSON (Object o) =
       Subscription <$> (SubscriptionId <$> o .: "id")
                    <*> o .: "plan"
                    <*> o .: "object"
                    <*> (fromSeconds <$> o .: "start")
                    <*> o .: "status"
                    <*> ((CustomerId <$> o .: "customer")
                    <|> (ExpandedCustomer <$> o .: "customer"))
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
                    <*> (H.toList <$> o .: "metadata")
   parseJSON _ = mzero

------------------------------------------------------------------------------
-- | Status of a `Subscription`
data SubscriptionStatus =
          Trialing
        | Active
        | PastDue
        | Canceled
        | UnPaid
        deriving (Show, Eq)

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
-- | Plans

------------------------------------------------------------------------------
-- | `PlanId` for a `Plan`
newtype PlanId = PlanId Text deriving (Show, Eq)

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
    , planDescription     :: Maybe Text
} deriving (Show, Eq)

------------------------------------------------------------------------------
-- | JSON Instance for `Plan`
instance FromJSON Plan where
   parseJSON (Object o) =
        Plan <$> o .: "interval"
             <*> o .: "name"
             <*> (fromSeconds <$> o .: "created")
             <*> o .: "amount"
             <*> (Currency <$> o .: "currency")
             <*> (PlanId <$> o .: "id")
             <*> o .: "object"
             <*> o .: "livemode"
             <*> o .:? "interval_count"
             <*> o .:? "trial_period_days"
             <*> (H.toList <$> o .: "metadata")
             <*> o .:? "statement_description"
   parseJSON _ = mzero

------------------------------------------------------------------------------
-- | `TrialPeriod` for a Plan
type TrialPeriod = UTCTime

------------------------------------------------------------------------------
-- | Interval for `Plan`s
data Interval = Week | Month | Year deriving Eq

------------------------------------------------------------------------------
-- | JSON Instance for `Interval`
instance FromJSON Interval where
   parseJSON (String "week") = pure Week
   parseJSON (String "month") = pure Month
   parseJSON (String "year") = pure Year
   parseJSON _ = mzero

------------------------------------------------------------------------------
-- | `Show` instance for `Interval`
instance Show Interval where
    show Week  = "week"
    show Month = "month"
    show Year  = "year"

------------------------------------------------------------------------------
-- | Coupon

------------------------------------------------------------------------------
-- | `Coupon` Duration
data Duration = Forever | Once | Repeating deriving Eq

------------------------------------------------------------------------------
-- | `Show` instance for `Duration`
instance Show Duration where
    show Forever   = "forever"
    show Once      = "once"
    show Repeating = "repeating"

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
    } deriving (Show, Eq)

------------------------------------------------------------------------------
-- | JSON Instance for `Coupon`
instance FromJSON Coupon where
   parseJSON (Object o) =
        Coupon <$> (CouponId <$> o .: "id")
               <*> (fromSeconds <$> o .: "created")
               <*> o .: "percent_off"
               <*> o .:? "amount_off"
               <*> (fmap Currency <$> o .:? "currency")
               <*> o .: "livemode"
               <*> o .: "duration"
               <*> (fmap fromSeconds <$> o .:? "redeem_by")
               <*> o .:? "max_redemptions"
               <*> o .:? "times_redeemed"
               <*> o .:? "duration_in_months"
               <*> o .: "valid"
               <*> (H.toList <$> o .: "metadata")
   parseJSON _ = mzero

------------------------------------------------------------------------------
-- | `CouponId` for a `Coupon`
newtype CouponId = CouponId Text deriving (Show, Eq)

------------------------------------------------------------------------------
-- | `AmountOff` for a `Coupon`
newtype AmountOff = AmountOff Int deriving (Show, Eq)

------------------------------------------------------------------------------
-- | `MaxRedemptions` for a `Coupon`
newtype MaxRedemptions = MaxRedemptions Int deriving (Show, Eq)

------------------------------------------------------------------------------
-- | `PercentOff` for a `Coupon`
newtype PercentOff = PercentOff Int deriving (Show, Eq)

------------------------------------------------------------------------------
-- | `RedeemBy` date for a `Coupon`
newtype RedeemBy = RedeemBy UTCTime deriving (Show, Eq)

------------------------------------------------------------------------------
-- | `DurationInMonths` for a `Coupon`
newtype DurationInMonths = DurationInMonths Int deriving (Show, Eq)

------------------------------------------------------------------------------
-- | `IntervalCount` for a `Coupon`
newtype IntervalCount   = IntervalCount Int deriving (Show, Eq)

------------------------------------------------------------------------------
-- | `TrialPeriodDays` for a `Coupon`
newtype TrialPeriodDays = TrialPeriodDays Int deriving (Show, Eq)

------------------------------------------------------------------------------
-- | `Amount` of a `Coupon`
type Amount = Int

------------------------------------------------------------------------------
-- | Discount

------------------------------------------------------------------------------
-- | `Discount` for `Coupon`
data Discount = Discount {
      discountCoupon       :: Coupon
    , discountStart        :: UTCTime
    , discountEnd          :: Maybe UTCTime
    , discountCustomer     :: CustomerId
    , discountObject       :: Text
    , discountSubscription :: Maybe Subscription
} deriving (Show, Eq)

------------------------------------------------------------------------------
-- | JSON Instance for `Discount`
instance FromJSON Discount where
    parseJSON (Object o) =
        Discount <$> o .: "coupon"
                 <*> (fromSeconds <$> o .: "start")
                 <*> (fmap fromSeconds <$> o .:? "end")
                 <*> ((CustomerId <$> o .: "customer")
                 <|> (ExpandedCustomer <$> o .: "customer"))
                 <*> o .: "object"
                 <*> o .:? "subscription"
    parseJSON _ = mzero

------------------------------------------------------------------------------
-- | Invoice

------------------------------------------------------------------------------
-- | `Invoice` for a `Coupon`
data InvoiceId =
    InvoiceId Text
  | ExpandedInvoice Invoice
  deriving (Show, Eq)

------------------------------------------------------------------------------
-- | JSON Instance for `InvoiceId`
instance FromJSON InvoiceId where
   parseJSON (String x)   = pure $ InvoiceId x
   parseJSON o@(Object _) = ExpandedInvoice <$> parseJSON o
   parseJSON _ = mzero

------------------------------------------------------------------------------
-- | `Invoice` Object
data Invoice = Invoice {
      invoiceDate                 :: UTCTime
    , invoiceId                   :: InvoiceId
    , invoicePeriodStart          :: UTCTime
    , invoicePeriodEnd            :: UTCTime
    , invoiceLineItems            :: StripeList InvoiceLineItem
    , invoiceSubTotal             :: Int
    , invoiceTotal                :: Int
    , invoiceCustomer             :: CustomerId
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
    , invoiceNextPaymentAttempt   :: UTCTime
    , invoiceWebHooksDeliveredAt  :: Maybe UTCTime
    , invoiceCharge               :: Maybe ChargeId
    , invoiceDiscount             :: Maybe Discount
    , invoiceApplicateFee         :: Maybe FeeId
    , invoiceSubscription         :: Maybe SubscriptionId
    , invoiceStatementDescription :: Maybe Text
    , invoiceDescription          :: Maybe Text
    , invoiceMetaData             :: MetaData
} deriving (Show, Eq)

------------------------------------------------------------------------------
-- | JSON Instance for `Invoice`
instance FromJSON Invoice where
   parseJSON (Object o) =
       Invoice <$> (fromSeconds <$> o .: "date")
               <*> (InvoiceId <$> o .: "id")
               <*> (fromSeconds <$> o .: "period_start")
               <*> (fromSeconds <$> o .: "period_end")
               <*> o .: "lines"
               <*> o .: "subtotal"
               <*> o .: "total"
               <*> ((CustomerId <$> o .: "customer")
               <|> (ExpandedCustomer <$> o .: "customer"))
               <*> o .: "object"
               <*> o .: "attempted"
               <*> o .: "closed"
               <*> o .: "forgiven"
               <*> o .: "paid"
               <*> o .: "livemode"
               <*> o .: "attempt_count"
               <*> o .: "amount_due"
               <*> (Currency <$> o .: "currency")
               <*> o .: "starting_balance"
               <*> o .:? "ending_balance"
               <*> (fromSeconds <$> o .: "next_payment_attempt")
               <*> (fmap fromSeconds <$> o .: "webhooks_delivered_at")
               <*> ((fmap ChargeId <$> o .:? "charge")
               <|> (fmap ExpandedCharge <$> o .:? "charge"))
               <*> o .:? "discount"
               <*> (fmap FeeId <$> o .:? "application_fee")
               <*> (fmap SubscriptionId <$> o .: "subscription")
               <*> o .:? "statement_description"
               <*> o .:? "description"
               <*> (H.toList <$> o .: "metadata")
   parseJSON _ = mzero

------------------------------------------------------------------------------
-- | Invoice Item

------------------------------------------------------------------------------
-- | `InvoiceItemId` for `InvoiceItem`
data InvoiceItemId
    = InvoiceItemId Text
    | ExpandedInvoiceItem InvoiceItem
      deriving (Eq, Show)

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
    , invoiceItemCustomer     :: CustomerId
    , invoiceItemDescription  :: Text
    , invoiceItemInvoice      :: Maybe InvoiceId
    , invoiceItemSubscription :: Maybe Subscription
    , invoiceItemMetaData     :: MetaData
    } deriving (Show, Eq)

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
                   <*> (Currency <$> o .: "currency")
                   <*> ((CustomerId <$> o .: "customer")
                   <|> (ExpandedCustomer <$> o .: "customer"))
                   <*> o .: "description"
                   <*> ((fmap InvoiceId <$> o .:? "invoice")
                   <|> (fmap ExpandedInvoice <$> o .:? "invoice"))
                   <*> o .:? "subscription"
                   <*> (H.toList <$> o .: "metadata")
   parseJSON _ = mzero

------------------------------------------------------------------------------
-- | `InvoiceLineItemId` for an `InvoiceLineItem`
newtype InvoiceLineItemId =
    InvoiceLineItemId Text deriving (Show, Eq)

------------------------------------------------------------------------------
-- | Type of `InvoiceItem`
data InvoiceLineItemType
    = InvoiceItemType |
     SubscriptionItemType
      deriving (Show,Eq)

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
    , invoiceLineItemDescription :: Maybe Text
    , invoiceLineItemMetaData    :: MetaData
  } deriving (Show, Eq)

------------------------------------------------------------------------------
-- | Period for an `InvoiceLineItem`
data Period = Period {
      start :: UTCTime
    , end   :: UTCTime
    } deriving (Show, Eq)

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
                       <*> (Currency <$> o .: "currency")
                       <*> o .: "proration"
                       <*> o .: "period"
                       <*> (fmap Quantity <$> o .:? "quantity")
                       <*> o .:? "plan"
                       <*> o .:? "description"
                       <*> (H.toList <$> o .: "metadata")
   parseJSON _ = mzero

------------------------------------------------------------------------------
-- | Dispute

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
    deriving (Show, Eq)

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
      deriving (Show, Eq)

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
      disputeChargeId            :: ChargeId
    , disputeAmount              :: Int
    , disputeCreated             :: UTCTime
    , disputeStatus              :: DisputeStatus
    , disputeLiveMode            :: Bool
    , disputeCurrency            :: Currency
    , disputeObject              :: DisputeReason
    , disputeReason              :: Text
    , disputeIsChargeRefundable  :: Bool
    , disputeBalanceTransactions :: [Text]
    , disputeEvidenceDueBy       :: UTCTime
    , disputeEvidence            :: Maybe Text
    , disputeMetaData            :: MetaData
    } deriving (Show, Eq)

------------------------------------------------------------------------------
-- | `Evidence` associated with a `Dispute`
newtype Evidence = Evidence Text deriving (Show, Eq)

------------------------------------------------------------------------------
-- | JSON Instance for `Dispute`
instance FromJSON Dispute where
    parseJSON (Object o) =
        Dispute <$> ((ChargeId <$> o .: "charge")
                <|> (ExpandedCharge <$> o .: "charge"))
                <*> o .: "amount"
                <*> (fromSeconds <$> o .: "created")
                <*> o .: "status"
                <*> o .: "live_mode"
                <*> (Currency <$> o .: "currency")
                <*> o .: "object"
                <*> o .: "reason"
                <*> o .: "is_charge_refundable"
                <*> o .: "balance_transactions"
                <*> (fromSeconds <$> o .: "evidence_due_by")
                <*> o .:? "evidence"
                <*> (H.toList <$> o .: "metadata")
    parseJSON _ = mzero

------------------------------------------------------------------------------
-- | Transfers

------------------------------------------------------------------------------
-- | `TransferId`
newtype TransferId =
  TransferId Text deriving (Show, Eq)

------------------------------------------------------------------------------
-- | Status of a `Transfer`
data TransferStatus =
    TransferPaid
  | TransferPending
  | TransferCanceled
  | TransferFailed
  deriving (Show, Eq)

------------------------------------------------------------------------------
-- | Type of a `Transfer`
data TransferType =
    CardTransfer
  | BankAccountTransfer
    deriving (Show, Eq)

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
    , transferCurrency             :: Text
    , transferStatus               :: TransferStatus
    , transferType                 :: TransferType
    , transferBalanceTransaction   :: TransactionId
    , transferDescription          :: Text
    , transferBankAccount          :: Account
    , transferFailureMessage       :: Maybe Text
    , transferFailureCode          :: Maybe Text
    , transferStatementDescription :: Maybe Text
    , transferRecipient            :: Maybe RecipientId
    , transferMetaData             :: MetaData
} deriving (Show, Eq)

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
                 <*> ((TransactionId <$> o .: "balance_transaction")
                 <|> (ExpandedTransaction <$> o .: "balance_transaction"))
                 <*> o .: "description"
                 <*> o .: "account"
                 <*> o .:? "failure_message"
                 <*> o .:? "failure_code"
                 <*> o .:? "statement_description"
                 <*> ((fmap RecipientId <$> o .:? "recipient")
                 <|> (fmap ExpandedRecipient <$> o .:? "recipient"))
                 <*> (H.toList <$> o .: "metadata")
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
} deriving (Show, Eq)

------------------------------------------------------------------------------
-- | `BankAccount` JSON Instance
instance FromJSON BankAccount where
   parseJSON (Object o) =
     BankAccount <$> (BankAccountId <$> o .: "id")
                 <*> o .: "object"
                 <*> o .: "last4"
                 <*> (Country <$> o .: "country")
                 <*> (Currency <$> o .: "currency")
                 <*> o .:? "status"
                 <*> o .:? "fingerprint"
                 <*> o .: "bank_name"
   parseJSON _ = mzero

------------------------------------------------------------------------------
-- | `BankAccountId` for `BankAccount`
newtype BankAccountId = BankAccountId Text
                        deriving (Show, Eq)

------------------------------------------------------------------------------
-- | `BankAccountStatus` Object
data BankAccountStatus =
  New | Validated | Verified | Errored
  deriving (Show, Eq)

------------------------------------------------------------------------------
-- | `BankAccountStatus` JSON instance
instance FromJSON BankAccountStatus where
   parseJSON (String "new") = pure $ New
   parseJSON (String "validated") = pure Validated
   parseJSON (String "verified") = pure Verified
   parseJSON (String "errored") = pure Errored
   parseJSON _ = mzero

------------------------------------------------------------------------------
-- | Routing Number for `BankAccount`
newtype RoutingNumber =
  RoutingNumber Text deriving (Show, Eq)

------------------------------------------------------------------------------
-- | Country of `BankAccount`
newtype Country       =
  Country Text deriving (Show, Eq)

------------------------------------------------------------------------------
-- | AccountNumber of `BankAccount`
newtype AccountNumber =
  AccountNumber Text deriving (Show, Eq)

------------------------------------------------------------------------------
-- | Recipients

------------------------------------------------------------------------------
-- | `FirstName` of a `Recipient`
newtype FirstName = FirstName Text deriving (Show, Eq)

------------------------------------------------------------------------------
-- | `LastName` of a `Recipient`
newtype LastName = LastName Text deriving (Show, Eq)

------------------------------------------------------------------------------
-- | Middle Initial of a `Recipient`
type MiddleInitial = Char

------------------------------------------------------------------------------
-- | `RecipientId` for a `Recipient`
data RecipientId =
      RecipientId Text
    | ExpandedRecipient Recipient
  deriving (Show, Eq)

------------------------------------------------------------------------------
-- | JSON Instance for `RecipientId`
instance FromJSON RecipientId where
   parseJSON (String x)   = pure $ RecipientId x
   parseJSON o@(Object _) = ExpandedRecipient <$> parseJSON o
   parseJSON _ = mzero

------------------------------------------------------------------------------
-- | `TaxID` of `Recipient`
type TaxID  = Text

------------------------------------------------------------------------------
-- | Type of `Recipient`
data RecipientType =
    Individual
  | Corporation deriving Eq

------------------------------------------------------------------------------
-- | `Show` instance for `RecipientType`
instance Show RecipientType where
    show Individual  = "individual"
    show Corporation = "corporation"

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
    , recipientDefaultCard   :: Maybe RecipientCardId
} deriving (Show, Eq)

------------------------------------------------------------------------------
-- | JSON Instance for `Recipient`
instance FromJSON Recipient where
   parseJSON (Object o) =
       Recipient <$> (RecipientId <$> o .: "id")
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
                 <*> ((fmap RecipientCardId <$> o .:? "default_card")
                 <|> (fmap ExpandedRecipientCard <$> o .:? "default_card"))
   parseJSON _ = mzero

------------------------------------------------------------------------------
-- | Application Fees

------------------------------------------------------------------------------
-- | ApplicationFee Object
data ApplicationFee = ApplicationFee {
      applicationFeeId                 :: Text
    , applicationFeeObjecet            :: Text
    , applicationFeeCreated            :: UTCTime
    , applicationFeeLiveMode           :: Bool
    , applicationFeeAmount             :: Int
    , applicationFeeCurrency           :: Text
    , applicationFeeRefunded           :: Bool
    , applicationFeeAmountRefunded     :: Int
    , applicationFeeRefunds            :: StripeList Refund
    , applicationFeeBalanceTransaction :: TransactionId
    , applicationFeeAccountId          :: AccountId
    , applicationFeeApplicationId      :: ApplicationId
    , applicationFeeChargeId           :: ChargeId
    , applicationFeeMetaData           :: MetaData
} deriving (Show, Eq)

------------------------------------------------------------------------------
-- | `ApplicationId` object
newtype ApplicationId =
  ApplicationId Text deriving (Show, Eq)

------------------------------------------------------------------------------
-- | JSON Instance for `ApplicationFee`
instance FromJSON ApplicationFee where
   parseJSON (Object o) =
       ApplicationFee <$> o .: "id"
                      <*> o .: "object"
                      <*> (fromSeconds <$> o .: "created")
                      <*> o .: "livemode"
                      <*> o .: "amount"
                      <*> o .: "currency"
                      <*> o .: "refunded"
                      <*> o .: "amount_refunded"
                      <*> o .: "refunds"
                      <*> ((TransactionId <$> o .: "balance_transaction")
                      <|> (ExpandedTransaction <$> o .: "balance_transaction"))
                      <*> ((AccountId <$> o .: "account")
                      <|> (ExpandedAccount <$> o .: "account"))
                      <*> (ApplicationId <$> o .: "application")
                      <*> ((ChargeId <$> o .: "charge")
                      <|> (ExpandedCharge <$> o .: "charge"))
                      <*> (H.toList <$> o .: "metadata")
   parseJSON _ = mzero

------------------------------------------------------------------------------
-- | `FeeId` for objects with Fees
newtype FeeId =
  FeeId Text
  deriving (Show, Eq)

------------------------------------------------------------------------------
-- | Application Fee Refunds

data ApplicationFeeRefund = ApplicationFeeRefund {
       applicationFeeRefundId                 :: RefundId
     , applicationFeeRefundAmount             :: Int
     , applicationFeeRefundCurrency           :: Text
     , applicationFeeRefundCreated            :: UTCTime
     , applicationFeeRefundObject             :: Text
     , applicationFeeRefundBalanceTransaction :: Maybe TransactionId
     , applicationFeeRefundFee                :: FeeId
     , applicationFeeRefundMetaData           :: MetaData
     } deriving (Show, Eq)

------------------------------------------------------------------------------
-- | JSON Instance for `ApplicationFeeRefund`
instance FromJSON ApplicationFeeRefund where
    parseJSON (Object o) = ApplicationFeeRefund
              <$> (RefundId <$> o .: "id")
              <*> o .: "amount"
              <*> o .: "currency"
              <*> (fromSeconds <$> o .: "created")
              <*> o .: "object"
              <*> ((fmap TransactionId <$> o .:? "balance_transaction")
              <|> (fmap ExpandedTransaction <$> o .:? "balance_transaction"))
              <*> (FeeId <$> o .: "fee")
              <*> (H.toList <$> o .: "metadata")
    parseJSON _ = mzero

------------------------------------------------------------------------------
-- | Account

------------------------------------------------------------------------------
-- | `AccountId` of an `Account`
data AccountId
  = AccountId Text
  | ExpandedAccount Account
  deriving (Show, Eq)

------------------------------------------------------------------------------
-- | JSON Instance for `AccountId`
instance FromJSON AccountId where
   parseJSON o@(Object _) = ExpandedAccount <$> parseJSON o
   parseJSON (String aid) = pure $ AccountId aid
   parseJSON _ = mzero

------------------------------------------------------------------------------
-- | `Account` Object
data Account = Account {
       accountId                  :: AccountId
     , accountEmail               :: Text
     , accountStatementDescriptor :: Maybe Text
     , accountDisplayName         :: Text
     , accountTimeZone            :: Text
     , accountDetailsSubmitted    :: Bool
     , accountChargeEnabled       :: Bool
     , accountTransferEnabled     :: Bool
     , accountCurrenciesSupported :: [Text]
     , accountDefaultCurrency     :: Text
     , accountCountry             :: Text
     , accountObject              :: Text
     , accountBusinessName        :: Text
     , accountBusinessURL         :: Text
     , accountBusinessLogo        :: Text
     , accountSupportPhone        :: Text
} deriving (Show, Eq)

------------------------------------------------------------------------------
-- | JSON Instance for `Account`
instance FromJSON Account where
   parseJSON (Object o) =
       Account <$> (AccountId <$> o .:  "id")
               <*> o .:  "email"
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
               <*> o .:  "business_name"
               <*> o .:  "business_url"
               <*> o .:  "business_logo"
               <*> o .:  "support_phone"
   parseJSON _ = mzero

------------------------------------------------------------------------------
-- | Balance

------------------------------------------------------------------------------
-- | `Balance` Object
data Balance = Balance {
      balancePending   :: [BalanceAmount]
    , balanceAvailable :: [BalanceAmount]
    , balanceLiveMode  :: Bool
    , balanceObject    :: Text
    } deriving (Show, Eq)

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
    , balanceCurrency :: Text
    } deriving (Show, Eq)

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
    , balanceTransactionCurrency       :: Text
    , balanceTransactionNet            :: Int
    , balanceTransactionType           :: Text
    , balanceTransactionCreated        :: UTCTime
    , balanceTransactionAvailableOn    :: UTCTime
    , balanceTransactionStatus         :: Text
    , balanceTransactionFee            :: Int
    , balanceTransactionFeeDetails     :: [FeeDetails]
    , balanceTransactionFeeSource      :: ChargeId
    , balanceTransactionFeeDescription :: Maybe Text
    } deriving (Show, Eq)

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
                          <*> ((ChargeId <$> o .: "source")
                          <|> (ExpandedCharge <$> o .: "source"))
                          <*> o .:? "description"
   parseJSON _ = mzero

------------------------------------------------------------------------------
-- | `TransactionId` of a `Transaction`
data TransactionId = TransactionId Text
                   | ExpandedTransaction BalanceTransaction
                   deriving (Show, Eq)

------------------------------------------------------------------------------
-- | JSON Instance for `TransactionId`
instance FromJSON TransactionId where
    parseJSON (String x)   = pure (TransactionId x)
    parseJSON v@(Object _) = ExpandedTransaction <$> parseJSON v
    parseJSON _            = mzero

------------------------------------------------------------------------------
-- | `FeeDetails` Object
data FeeDetails = FeeDetails {
      feeDetailsAmount   :: Int
    , feeDetailsCurrency :: Text
    , feeType            :: Text
    , feeDescription     :: Text
    , feeApplication     :: Maybe Text
} deriving (Show, Eq)

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
-- | Events

------------------------------------------------------------------------------
-- | `EventId` of an `Event`
newtype EventId = EventId Text deriving (Show, Eq)

------------------------------------------------------------------------------
-- | `Event` Object
data Event = Event {
      eventId       :: EventId
    , eventCreated  :: UTCTime
    , eventLiveMode :: Text
    , eventType     :: Text
} deriving (Show, Eq)

------------------------------------------------------------------------------
-- | JSON Instance for `Event`
instance FromJSON Event where
   parseJSON (Object o) =
       Event <$> (EventId <$> o .: "id")
             <*> (fromSeconds <$> o .: "created")
             <*> o .: "livemode"
             <*> o .: "type"
   parseJSON _ = mzero

------------------------------------------------------------------------------
-- | Token

------------------------------------------------------------------------------
-- | `TokenId` of an `Token`
newtype TokenId =
    TokenId Text
    deriving (Show, Eq)

------------------------------------------------------------------------------
-- | Type of `Token`
data TokenType = TokenCard
               | TokenBankAccount
                 deriving (Show, Eq)

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
} deriving (Show, Eq)

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
-- | Generic handling of `Stripe` list JSON results
data StripeList a = StripeList {
      list       :: [a]
    , stripeUrl  :: Text
    , object     :: Text
    , totalCount :: Maybe Int
    , hasMore    :: Bool
    } deriving (Show, Eq)

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
type Limit             = Maybe Int

------------------------------------------------------------------------------
-- | Pagination Option for `StripeList`
type StartingAfter a = Maybe a

------------------------------------------------------------------------------
-- | Pagination Option for `StripeList`
type EndingBefore a = Maybe a

------------------------------------------------------------------------------
-- | JSON returned from a `Stripe` deletion request
data StripeDeleteResult = StripeDeleteResult {
      deleted   :: Bool
    , deletedId :: Maybe Text
    } deriving (Show, Eq)

------------------------------------------------------------------------------
-- | JSON Instance for `StripeDeleteResult`
instance FromJSON StripeDeleteResult where
   parseJSON (Object o) =
       StripeDeleteResult <$> o .: "deleted"
                          <*> o .:? "id"  
   parseJSON _ = mzero

------------------------------------------------------------------------------
-- | Type of MetaData for use on `Stripe` objects
type MetaData = [ (Text,Text) ]

------------------------------------------------------------------------------
-- | Type of Expansion Parameters for use on `Stripe` objects
type ExpandParams = [Text]

------------------------------------------------------------------------------
-- | Generic ID for use in constructing API Calls
type ID    = Text

------------------------------------------------------------------------------
-- | Generic URL for use in constructing API Calls
type URL   = Text

------------------------------------------------------------------------------
-- | Generic URL for use in constructing API Calls
type Name  = Text

------------------------------------------------------------------------------
-- | Generic Description for use in constructing API Calls
type Description   = Text

------------------------------------------------------------------------------
-- | Generic type to handle `Currency`
newtype Currency        = Currency Text deriving (Show, Eq)

------------------------------------------------------------------------------
-- | Generic `Quantity` type to be used with `Customer`,
-- `Subscription` and `InvoiceLineItem` API requests
newtype Quantity = Quantity Int deriving (Show, Eq)

------------------------------------------------------------------------------
-- | `Email` associated with a `Customer`, `Recipient` or `Charge`
newtype Email = Email Text deriving (Show, Eq)
