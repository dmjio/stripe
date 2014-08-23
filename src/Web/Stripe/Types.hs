{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Web.Stripe.Types where

import           Control.Applicative
import           Data.Aeson
import           Data.Text           (Text)
import           Data.Time
import           Data.Vector
import           Web.Stripe.Util

newtype ChargeId = ChargeId Text deriving (Show, Eq)

data Charge = Charge {
      chargeId         :: Text
    , chargeObject     :: Text
    , chargeCreated    :: UTCTime
    , chargeLiveMode   :: Bool
    , chargePaid       :: Bool
    , chargeAmount     :: Int
    , chargeCurrency   :: Text
    , chargeRefunded   :: Bool
    , chargeCreditCard :: Card
    , chargeCaptured   :: Bool
    , chargeBalanceTransaction :: TransactionId
    , chargeFailureMessage       :: Maybe Text
    , chargeFailureCode          :: Maybe Text
    , chargeAmountRefunded       :: Int
    , chargeCustomerId           :: Maybe CustomerId 
    , chargeInvoice              :: Maybe InvoiceId
    , chargeDescription          :: Maybe Text
    , chargeDispute              :: Maybe Text
    , chargeStatementDescription :: Maybe Text
    , chargeReceiptEmail         :: Maybe Text
    } deriving Show

newtype StatementDescription = StatementDescription Text deriving (Show, Eq)
type Capture = Bool

newtype ReceiptEmail = ReceiptEmail Text deriving (Show, Eq)

-- instance FromJSON [Charge] where
--     parseJSON (Object o) = do
--       Array d <- o .: "data"
--       return d

instance FromJSON Charge where
    parseJSON (Object o) =
        Charge <$> o .: "id"
               <*> o .: "object"
               <*> (fromSeconds <$> o .: "created")
               <*> o .: "livemode"
               <*> o .: "paid"
               <*> o .: "amount"
               <*> o .: "currency"
               <*> o .: "refunded"
               <*> o .: "card"
               <*> o .: "captured"
               <*> (TransactionId <$> o .: "balance_transaction")
               <*> o .:? "failure_message"
               <*> o .:? "failure_code"
               <*> o .: "amount_refunded"
               <*> (fmap CustomerId <$> o .:? "customer")
               <*> (fmap InvoiceId <$> o .:? "invoice")
               <*> o .:? "description"
               <*> o .:? "dispute"
               <*> o .:? "statement_description"
               <*> o .:? "receipt_email"

-- Customer --
newtype CustomerId = CustomerId Text deriving (Show)

data Customer = Customer {
      customerCreated        :: UTCTime
    , customerId             :: Text
    , delinquent             :: Bool
    , customerDescription    :: Maybe Text
    , customerEmail          :: Maybe Text
    , customerAccountBalance :: Maybe Int
    , customerCurrency       :: Maybe Text
    , customerDiscount       :: Maybe Discount
    } deriving (Show, Eq)

instance FromJSON Customer where
    parseJSON (Object o)
        = Customer
           <$> (fromSeconds <$> o .: "created")
           <*> o .: "id"
           <*> o .: "delinquent"
           <*> o .:? "description"
           <*> o .:? "email"
           <*> o .:? "account_balance"
           <*> o .:? "currency"
           <*> o .:? "discount"

---- ==== Card ==== -----
newtype CardId         = CardId Text deriving (Show, Eq)
newtype CardNumber     = CardNumber Int deriving (Show, Eq, Ord)
newtype ExpMonth       = ExpMonth Int deriving (Show, Eq, Ord)
newtype ExpYear        = ExpYear Int deriving (Show, Eq, Ord)
newtype CVC            = CVC Int deriving (Show, Eq, Ord)
newtype AddressCity    = AddressCity Text deriving (Show, Eq)
newtype AddressCountry = AddressCountry Text deriving (Show, Eq)
newtype AddressLine1   = AddressLine1 Text deriving (Show, Eq)
newtype AddressLine2   = AddressLine2 Text deriving (Show, Eq)
newtype AddressState   = AddressState Text deriving (Show, Eq)
newtype AddressZip     = AddressZip Text deriving (Show, Eq)
newtype EndingBefore   = EndingBefore Text deriving (Show, Eq)
newtype StartingAfter  = StartingAfter Text deriving (Show, Eq)
newtype Limit          = Limit Int deriving (Show, Eq)

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

-- Token --
newtype TokenId = TokenId Text deriving (Show, Eq, Ord)

data TokenType = TokenCard | TokenBankAccount deriving (Show, Eq)

instance FromJSON TokenType where
   parseJSON (String "bank_account") = pure TokenBankAccount
   parseJSON (String "card") = pure TokenCard
   parseJSON _ = error "Additional token type not documented in Stripe's API"

data Token = Token {
      tokenId      :: TokenId
    , tokenCreated :: UTCTime
    , tokenUsed    :: Bool
    , tokenType    :: TokenType
    , tokenCard    :: Card
} deriving (Show)

instance FromJSON Token where
   parseJSON (Object o) = undefined
       -- Token <$> (TokenId <$> (o .: "id"))
       --       <*> o .: "livemode"
       --       <*> (fromSeconds <$> o .: "created")
       --       <*> o .: "used"
       --       <*> o .: "type"
       --       <*> o .: "card"

---- == Invoice Item == ------
data InvoiceLineItem = InvoiceLineItem {
      invoiceLineItemId          :: Text
    , invoiceLineItemLiveMode    :: Bool
    , invoiceLineItemAmount      :: Int
    , invoiceLineItemCurrency    :: Text
    , invoiceLineItemProration   :: Bool
    , invoiceLineItemStart       :: UTCTime
    , invoiceLineItemEnd         :: UTCTime
    , invoiceLineItemQuantity    :: Maybe Int
    , invoiceLineItemPlan        :: Maybe Plan
    , invoiceLineItemDescription :: Maybe Text
  } deriving Show

---- == Invoice == ------
data Invoice = Invoice {
      invoiceDate                 :: UTCTime
    , invoiceId                   :: Text
    , invoicePeriodStart          :: UTCTime
    , invoicePeriodEnd            :: UTCTime
--    , invoiceLineItems            :: StripeList InvoiceLineItem
    , invoiceSubTotal             :: Int
    , invoiceTotal                :: Int
    , invoiceCustomer             :: Text
    , invoiceAttempted            :: Bool
    , invoiceClosed               :: Bool
    , invoiceForgiven             :: Bool
    , invoicePaid                 :: Bool
    , invoiceLiveMode             :: Bool
    , invoiceAttemptCount         :: Int
    , invoiceAmountDue            :: Int
    , invoiceCurrency             :: Text
    , invoiceStartingBalance      :: Int
    , invoiceEndingBalance        :: Maybe Int
    , invoiceNextPaymentAttempt   :: UTCTime
    , invoiceWebHooksDeliveredAt  :: UTCTime
    , invoiceCharge               :: Maybe ChargeId
    , invoiceDiscount             :: Maybe Text
    , invoiceApplicateFee         :: Maybe ApplicationFee
    , invoiceSubscription         :: SubscriptionId
    , invoiceStatementDescription :: Maybe Text
    , invoiceDescription          :: Maybe Text
} deriving Show

newtype SubscriptionId = SubscriptionId Text deriving (Show, Eq)

instance FromJSON Invoice where
   parseJSON (Object o) = undefined

instance FromJSON InvoiceLineItem where
   parseJSON (Object o) = undefined

-- https://stripe.com/docs/api#retrieve_invoiceitem
newtype InvoiceId = InvoiceId Text deriving (Show, Eq)
-- Invoice Item --
newtype InvoiceItemId = InvoiceItemId Text deriving (Eq, Show)

--- Discount --

data Discount = Discount {
      discountStart    :: Int
    , discountEnd      :: Int
    , discountCustomer :: Text
} deriving (Show, Eq)



instance FromJSON Discount where
    parseJSON (Object o) =
        Discount <$> o .: "start"
                 <*> o .: "end"
                 <*> o .: "customer"


-- Coupon --
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
      couponId               :: Text
    , couponCreated          :: UTCTime
    , couponPercentOff       :: Int
    , couponAmountOff        :: Maybe Int
    , couponCurrency         :: Maybe Text
    , couponLiveMode         :: Bool
    , couponDuration         :: Duration
    , couponRedeemBy         :: Maybe UTCTime
    , couponMaxRedemptions   :: Maybe Int
    , couponTimesRedeemed    :: Maybe Int
    , couponDurationInMonths :: Maybe Int
    , couponValid            :: Bool
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

-- Plan --

newtype PlanId          = PlanId Text deriving (Show, Eq)
newtype Name            = Name Text deriving (Show, Eq)
newtype Currency        = Currency Text deriving (Show, Eq)
newtype IntervalCount   = IntervalCount Int deriving (Show, Eq)
newtype TrialPeriodDays = TrialPeriodDays Int deriving (Show, Eq)
newtype Description     = Description Text deriving (Show, Eq)
newtype Amount          = Amount Int deriving (Show, Eq)

data Interval = Week | Month | Year deriving (Eq)

instance Show Interval where
    show Week  = "week"
    show Month = "month"
    show Year  = "year"

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


--- Account ---
newtype AccountId = AccountId Text deriving (Show, Eq)

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
} deriving (Show, Eq)

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

-- Application Fee --
data ApplicationFee = ApplicationFee {
      applicationFeeId             :: Text
    , applicationFeeCreated        :: UTCTime
    , applicationFeeLiveMode       :: Bool
    , applicationFeeAmount         :: Int
    , applicationFeeCurrency       :: Text
    , applicationFeeRefunded       :: Bool
    , applicationFeeAmountRefunded :: Int
} deriving (Show)

instance FromJSON ApplicationFee where
   parseJSON (Object o) = undefined

newtype FeeId = FeeId { feeId :: Text } deriving (Show, Eq)

-- Events --
newtype EventId = EventId Text deriving (Show, Eq)

data EventType = ChargeEvent
   | RefundEvent
   | AdjustmentEvent
   | ApplicationFeeEvent
   | ApplicationFeeRefundEvent
   | TransferEvent
   | TransferFailureEvent
      deriving (Show, Eq)

data Event = Event {
      eventId      :: EventId
    , eventCreated :: UTCTime
    , eventType    :: Text
} deriving (Show)

instance FromJSON Event where
   parseJSON (Object o) = undefined

-- Balance --
data BalanceAmount = BalanceAmount {
      balanceAmount   :: Int
    , balanceCurrency :: Text
    } deriving Show

data Balance = Balance {
      balancePending   :: [BalanceAmount]
    , balanceAvailable :: [BalanceAmount]
    } deriving Show

data BalanceTransaction = BalanceTransaction {
      balanceTransactionId             :: TransactionId
    , balanceTransactionAmount         :: Int
    , balanceTransactionCurrency       :: Text
    , balanceTransactionNet            :: Int
    , balanceTransactionType           :: Text
    , balanceTransactionCreated        :: UTCTime
    , balanceTransactionAvailableOn    :: UTCTime
    , balanceTransactionStatus         :: Text
    , balanceTransactionFee            :: Int
    , balanceTransactionFeeDetails     :: [FeeDetails]
    , balanceTransactionFeeSource      :: Text
    , balanceTransactionFeeDescription :: Text
    } deriving Show

data FeeDetails = FeeDetails {
      feeDetailsAmount   :: Int
    , feeDetailsCurrency :: Text
    , feeType            :: Text
    , feeDescription     :: Text
} deriving (Show)

newtype TransactionId = TransactionId Text deriving (Show, Eq)

instance FromJSON FeeDetails where
   parseJSON (Object o) =
       FeeDetails <$> o .: "amount"
                  <*> o .: "currency"
                  <*> o .: "type"
                  <*> o .: "description"

instance FromJSON BalanceAmount where
   parseJSON (Object o) =
       BalanceAmount <$> o .: "amount"
                     <*> o .: "currency"
instance FromJSON Balance where
   parseJSON (Object o) =
       Balance <$> o .: "pending"
               <*> o .: "available"

instance FromJSON BalanceTransaction where
   parseJSON (Object o) =
       BalanceTransaction <$> (TransactionId <$> o .: "id")
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
                          <*> o .: "description"
