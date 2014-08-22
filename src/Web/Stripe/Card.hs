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
getCard :: CustomerId -> CardId -> Stripe Card
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

--- ===== plans ====== --

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
data Discount = Discount {
      discountCoupon :: Coupon
    , discountCustomerId :: CustomerId
    , discountStart :: UTCTime
    , discountEnd :: Maybe UTCTime
    , discountSubscription ::  Maybe SubscriptionId
} deriving (Show)

deleteCustomerDiscount :: CustomerId -> IO (Either StripeError StripeResult)
deleteCustomerDiscount (CustomerId customerId) = sendStripeRequest config req []
  where req = StripeRequest DELETE url 
        url = "customers/" <> customerId <> "/discount"

deleteSubscriptionDiscount ::
  CustomerId -> 
  SubscriptionId -> 
  IO (Either StripeError StripeResult)
deleteSubscriptionDiscount (CustomerId customerId) (SubscriptionId subId) =
    sendStripeRequest config req []
  where req = StripeRequest DELETE url 
        url = T.concat ["customers/"
                       , customerId
                       , "/subscriptions/"
                       , subId
                       , "/discount"
                       ]

-- Invoices
data InvoiceLineItem = InvoiceLineItem {
      invoiceLineItemId :: Text
    , invoiceLineItemLiveMode :: Bool
    , invoiceLineItemAmount :: Int
    , invoiceLineItemCurrency :: Text
    , invoiceLineItemProration :: Bool
    , invoiceLineItemStart :: UTCTime
    , invoiceLineItemEnd :: UTCTime
    , invoiceLineItemQuantity :: Maybe Int
    , invoiceLineItemPlan :: Maybe Plan
    , invoiceLineItemDescription :: Maybe Text
  } deriving Show


data Invoice = Invoice {
      invoiceDate :: UTCTime
    , invoiceId :: InvoiceId
    , invoicePeriodStart :: UTCTime
    , invoicePeriodEnd :: UTCTime
    , invoiceLineItems :: StripeList InvoiceLineItem
    , invoiceSubTotal :: Int
    , invoiceTotal :: Int
    , invoiceCustomer :: CustomerId
    , invoiceAttempted :: Bool
    , invoiceClosed :: Bool
    , invoiceForgiven :: Bool
    , invoicePaid :: Bool
    , invoiceLiveMode :: Bool
    , invoiceAttemptCount :: Int
    , invoiceAmountDue :: Int
    , invoiceCurrency :: Text
    , invoiceStartingBalance :: Int
    , invoiceEndingBalance :: Maybe Int
    , invoiceNextPaymentAttempt :: UTCTime
    , invoiceWebHooksDeliveredAt :: UTCTime
    , invoiceCharge :: Maybe ChargeId
    , invoiceDiscount :: Maybe Text
    , invoiceApplicateFee :: Maybe ApplicationFee
    , invoiceSubscription :: SubscriptionId
    , invoiceStatementDescription :: Maybe Text
    , invoiceDescription :: Maybe Text
} deriving Show

instance FromJSON Invoice where
   parseJSON (Object o) = undefined

instance FromJSON InvoiceLineItem where
   parseJSON (Object o) = undefined

-- https://stripe.com/docs/api#retrieve_invoiceitem
newtype InvoiceId = InvoiceId Text deriving (Show, Eq)

-- unsure this one is correct
getInvoice :: InvoiceId -> IO (Either StripeError Invoice)
getInvoice (InvoiceId invoiceId) =
    sendStripeRequest config req []
  where req = StripeRequest GET url 
        url = "invoices/" <> invoiceId

-- see optional params
getInvoiceLineItems :: InvoiceId -> IO (Either StripeError (StripeList InvoiceLineItem))
getInvoiceLineItems (InvoiceId invoiceId) =
    sendStripeRequest config req []
  where req = StripeRequest GET url
        url = T.concat ["invoices/"
                       , invoiceId
                       , "/lines"
                       ]

createInvoice :: CustomerId -> IO (Either StripeError Invoice)
createInvoice (CustomerId customerId) =
    sendStripeRequest config req params
  where req = StripeRequest POST "invoices"
        params = [ ("customer", T.encodeUtf8 customerId) ]

payInvoice :: InvoiceId -> IO (Either StripeError Invoice)
payInvoice (InvoiceId invoiceId) =
    sendStripeRequest config req []
  where req = StripeRequest POST url 
        url = "invoices/" <> invoiceId <> "/pay"

-- see optional params
updateInvoice :: InvoiceId -> IO (Either StripeError Invoice)
updateInvoice (InvoiceId invoiceId) =
    sendStripeRequest config req  []
  where req = StripeRequest POST url 
        url = "invoices/" <> invoiceId

-- -- see optional for customer and date
getInvoices :: IO (Either StripeError (StripeList Invoice))
getInvoices = sendStripeRequest config req []
  where req = StripeRequest GET url 
        url = "invoicesadf"

-- -- see customer and subscription as optional
getUpcomingInvoice :: CustomerId -> IO (Either StripeError Invoice)
getUpcomingInvoice (CustomerId customerId) =
    sendStripeRequest config req []
  where req = StripeRequest GET url
        url = "invoices/upcoming?customer=" <> customerId

-- ---- Create Invoice Items
createInvoiceItem :: CustomerId -> IO (Either StripeError InvoiceLineItem)
createInvoiceItem (CustomerId customerId) =
    sendStripeRequest config req params
  where req = StripeRequest POST "invoiceitems"
        params = [ ("customer", T.encodeUtf8 customerId)
                 , ("amount", "1000")
                 , ("currency", "usd")
                 ]

newtype InvoiceLineItemId = InvoiceLineItemId Text deriving (Eq, Show)

getInvoiceItem :: InvoiceLineItemId -> IO (Either StripeError InvoiceLineItem)
getInvoiceItem (InvoiceLineItemId itemId) =
    sendStripeRequest config req []
  where req = StripeRequest GET url 
        url = "invoiceitems/" <> itemId

-- see additional parameters
updateInvoiceItem :: InvoiceLineItemId -> IO (Either StripeError InvoiceLineItem)
updateInvoiceItem (InvoiceLineItemId itemId) =
    sendStripeRequest config req []
  where req = StripeRequest POST url 
        url = "invoiceitems/" <> itemId

deleteInvoiceItem :: InvoiceLineItemId -> IO (Either StripeError StripeResult)
deleteInvoiceItem (InvoiceLineItemId itemId) =
    sendStripeRequest config req []
  where req = StripeRequest DELETE url 
        url = "invoiceitems/" <> itemId

-- Disputes

data Dispute = Dispute {
      
} deriving (Show)

instance FromJSON Dispute where
    parseJSON (Object o) = undefined

updateDispute :: ChargeId -> IO (Either StripeError Dispute)
updateDispute (ChargeId chargeId) = sendStripeRequest config req []
  where req = StripeRequest POST url
        url = "charges/" <> chargeId <> "/dispute"

-- test this
closeDispute :: ChargeId -> IO (Either StripeError Dispute)
closeDispute (ChargeId chargeId) = sendStripeRequest config req []
  where req = StripeRequest POST url
        url = "charges/" <> chargeId <> "/dispute/close"

-- transfers

newtype RecipientId = RecipientId { recipientId :: Text } deriving (Show, Eq)
newtype TransferId = TransferId Text deriving (Show, Eq)

data TransferStatus = Paid | Pending | Canceled | Failed deriving (Show, Eq)
data TransferType = CardTransfer | BankAccountTransfer deriving (Show, Eq)

instance FromJSON TransferType where
    parseJSON (String "card")         = pure CardTransfer
    parseJSON (String "bank_account") = pure BankAccountTransfer

instance FromJSON TransferStatus where
    parseJSON (String "paid")     = pure Paid
    parseJSON (String "pending")  = pure Pending
    parseJSON (String "canceled") = pure Canceled
    parseJSON (String "failed")   = pure Failed

data Transfer = Transfer {
      transferId :: TransferId
    , transferCreated :: UTCTime
    , transferDate :: UTCTime
    , transferAmount :: Int
    , transferCurrency :: Text
    , transferStatus :: TransferStatus
    , transferType :: TransferType
    , transferBalanceTransaction :: Text -- what??
    , transferDescription :: Text
} deriving (Show)

instance FromJSON Transfer where
    parseJSON (Object o) = undefined

createTransfer :: RecipientId -> IO (Either StripeError Transfer)
createTransfer (RecipientId recipientId) = sendStripeRequest config req params
  where req = StripeRequest POST "transfers" 
        params = [ ("amount", "400")
                 , ("currency", "usd")
                 , ("recipient", T.encodeUtf8 recipientId)
                 ]

getTransfer :: TransferId -> IO (Either StripeError Transfer)
getTransfer (TransferId transferId) = sendStripeRequest config req []
  where req = StripeRequest GET url 
        url = "transfers/" <> transferId

-- -- see additional description and metadata
updateTransfer :: TransferId -> IO (Either StripeError Transfer)
updateTransfer (TransferId transferId) = sendStripeRequest config req []
  where req = StripeRequest POST url
        url = "transfers/" <> transferId

cancelTransfer :: TransferId -> IO (Either StripeError Transfer)
cancelTransfer (TransferId transferId) = sendStripeRequest config req []
  where req = StripeRequest POST url 
        url = "transfers/" <> transferId <> "/cancel"

-- -- see optional params
getTransfers :: IO (Either StripeError (StripeList Transfer))
getTransfers = sendStripeRequest config req []
  where req = StripeRequest GET url 
        url = "transfers"

-- recipients

data RecipientType = Individual | Corporation deriving (Eq, Show)

data Recipient = Recipient { } deriving (Show)

instance FromJSON Recipient where
   parseJSON (Object o) = undefined


-- Text should be name?
createRecipient :: Text -> RecipientType -> IO (Either StripeError Recipient)
createRecipient name recipientType  = sendStripeRequest config req params
  where req    = StripeRequest POST "recipients" 
        params = [ ("name", T.encodeUtf8 name)
                 , ("type", if recipientType == Individual
                            then "individual"
                            else "corporation")
                 ]

getRecipient :: RecipientId -> IO (Either StripeError Recipient)
getRecipient (RecipientId recipientId) = sendStripeRequest config req []
  where req =  StripeRequest GET url 
        url = "recipients/" <> recipientId

-- -- see optional
updateRecipient :: RecipientId -> IO (Either StripeError Recipient)
updateRecipient (RecipientId recipientId) = sendStripeRequest config req []
  where req = StripeRequest POST url
        url = "recipients/" <> recipientId

deleteRecipient :: RecipientId -> IO (Either StripeError Recipient)
deleteRecipient (RecipientId recipientId) = sendStripeRequest config req []
  where req =  StripeRequest DELETE url 
        url = "recipients/" <> recipientId

-- -- get all recipients
getRecipients :: IO (Either StripeError (StripeList Recipient))
getRecipients = sendStripeRequest config req []
  where req =  StripeRequest GET "recipients" 

-- Application Fees

data ApplicationFee = ApplicationFee {} deriving (Show)
instance FromJSON ApplicationFee where
   parseJSON (Object o) = undefined

newtype FeeId = FeeId { feeId :: Text } deriving (Show, Eq)

getApplicationFee :: FeeId -> IO (Either StripeError ApplicationFee)
getApplicationFee (FeeId feeId) = sendStripeRequest config req []
  where req =  StripeRequest GET url 
        url = "application_fees/" <> feeId

refundApplicationFee :: FeeId -> IO (Either StripeError ApplicationFee)
refundApplicationFee (FeeId feeId) = sendStripeRequest config req  []
  where req =  StripeRequest POST url 
        url = "application_fees/" <> feeId <> "/refund"

-- see optional
getApplicationFees :: FeeId -> IO (Either StripeError (StripeList ApplicationFee))
getApplicationFees (FeeId feeId) = sendStripeRequest config req []
  where req =  StripeRequest GET "application_fees"  

-- Application Fee Refund
-- newtype ApplicationFeeRefundId = ApplicationFeeRefundId Text deriving (Show, Eq)

-- data ApplicationFeeRefund = ApplicationFeeRefund {  
--       applicationFeeRefundId :: ApplicationFeeRefundId
--     , applicationFeeRefundAmount :: Int
--     , applicationFeeRefundCreated :: UTCTime
--     , applicationFeeRefundCurrency :: Text
--     , applicationFeeRefundBalanceTransaction :: Maybe Text
--     , applicationFeeRefundFee :: FeeId
--     } deriving Show

-- instance FromJSON ApplicationFeeRefund where
--    parseJSON (Object o) = undefined  

-- Account

-- balance
getAccountBalance :: IO (Either StripeError Account)
getAccountBalance = sendStripeRequest config req params
  where req    =  StripeRequest GET "balance"  
        params = []

-- newtype TransactionId = TransactionId { transactionId :: Text } deriving (Show, Eq)

-- getBalanceTransaction :: TransactionId -> IO ()
-- getBalanceTransaction (TransactionId transactionId) =
--     sendStripeRequest req config
--   where req =  StripeRequest GET url []
--         url = "balance/history/" <> transactionId
