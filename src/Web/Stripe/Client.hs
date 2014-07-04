{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Web.Stripe.Client
    ( sendStripeRequest
    , StripeRequest (..)
    , StripeConfig (..)
    ) where

import           Control.Applicative
import           Data.Aeson
import           Data.Aeson.Types
import           Data.HashMap.Strict             as H
import           Data.Maybe                      (mapMaybe)
import           Data.Monoid                     ((<>))
import           Data.Text                       (Text)
import           Data.Time.Clock
import           Data.Typeable
import           Network.Http.Client
import           OpenSSL                         (withOpenSSL)
import           Text.Printf
import           Web.Stripe.Internal.Class
import           Web.Stripe.Internal.StripeError
import           Web.Stripe.Util

import qualified Data.ByteString                 as S
import qualified Data.ByteString.Lazy            as BL
import qualified Data.ByteString.Lazy.Char8      as BL8
import qualified Data.Text                       as T
import qualified Data.Text.Encoding              as T
import qualified System.IO.Streams               as Streams

type URL = S.ByteString

data StripeRequest = StripeRequest
    { method :: Method
    , url    :: Text
    } deriving (Show)

data StripeConfig = StripeConfig
    { secretKey  :: S.ByteString
    , apiVersion :: S.ByteString
    } deriving (Show)

sendStripeRequest :: (URLEncodeable a, FromJSON b) =>
                     StripeConfig ->
                     StripeRequest ->
                     a ->
                     IO (Either StripeError b)
sendStripeRequest StripeConfig{..} StripeRequest{..} params = withOpenSSL $ do
  ctx <- baselineContextSSL
  c <- openConnectionSSL ctx "api.stripe.com" 443
  q <- buildRequest $ do
          http method $ "/v1/" <> T.encodeUtf8 url
          setAuthorizationBasic secretKey ""
          setContentType "application/x-www-form-urlencoded"
          setHeader "Stripe-Version" apiVersion
  body <- Streams.fromByteString $ convertToString $ formEncode params
  sendRequest c q (inputStreamBody body)
  receiveResponse c $ \response inputStream ->
           Streams.read inputStream >>=
                  maybe (error "couldn't read stream") (handleStream response)
      where
        handleStream p x = do
          print (x, p)
          return $ case getStatusCode p of
                     c | c == 200 -> case decodeStrict x of
                                       Nothing -> error "oops"
                                       Just res -> Right res
                       | c >= 400 -> case decodeStrict x :: Maybe StripeError of
                                       Nothing -> error "oops"
                                       Just res  -> Left res
                       | otherwise -> undefined

config = StripeConfig "sk_test_zvqdM2SSA6WwySqM6KJQrqpH" "2014-03-28"

-- util
toParams :: [(a, Maybe b)] -> [(a, b)]
toParams = mapMaybe . uncurry $ fmap . (,)
--

-- -- create a refund
-- createRefund :: Charge -> IO ()
-- createRefund (Charge chargeId) = sendStripeRequest req config
--   where req = StripeRequest POST url []
--         url = "charges/" <> chargeId <> "/refunds"

-- getRefund :: Charge -> RefundId -> IO ()
-- getRefund (Charge chargeId) (RefundId refId) = sendStripeRequest req config
--   where req = StripeRequest GET url []
--         url = "charges/" <> chargeId <> "/refunds/" <> refId

-- -- optional metadata
-- updateRefund :: Charge -> RefundId -> IO ()
-- updateRefund (Charge chargeId) (RefundId refId) = sendStripeRequest req config
--   where req = StripeRequest POST url []
--         url = "charges/" <> chargeId <> "/refunds/" <> refId

-- -- optional, limit, etc
-- getRefunds :: Charge -> IO ()
-- getRefunds (Charge chargeId) = sendStripeRequest req config
--   where req = StripeRequest GET url []
--         url = "charges/" <> chargeId <> "/refunds"
-- ---

-- ---- Customer

-- -- Create card
-- newtype Card = Card { cardId :: Text } deriving (Show, Eq)

-- createCard :: CustomerId -> Token -> IO ()
-- createCard (CustomerId cid) (Token token_id) = sendStripeRequest req config
--   where req = StripeRequest POST url []
--         url = "customers/" <> cid
--         params = [("card", token_id)] -- card is required

-- -- See all the optional arguments here, lots of them
-- updateCard :: CustomerId -> Card -> IO ()
-- updateCard (CustomerId custId) (Card cardId) = sendStripeRequest req config
--   where req = StripeRequest DELETE url []
--         url = "customers/" <> custId <> "/cards/" <> cardId

-- deleteCard :: CustomerId -> Card -> IO ()
-- deleteCard (CustomerId custId) (Card cardId) = sendStripeRequest req config
--   where req = StripeRequest DELETE url []
--         url = "customers/" <> custId <> "/cards/" <> cardId

-- -- optional args
-- -- ending_before
-- -- limit
-- -- starting_after

-- getCards :: CustomerId -> IO ()
-- getCards (CustomerId custId) = sendStripeRequest req config
--   where req = StripeRequest GET url []
--         url = "customers/" <> custId <> "/cards"



-- --- Subscriptions
-- newtype SubscriptionId = SubscriptionId { subscriptionId :: Text } deriving (Show, Eq)
-- createSubscription :: CustomerId -> PlanId -> IO ()
-- createSubscription (CustomerId custId) (PlanId plan) = sendStripeRequest req config
--   where req = StripeRequest POST url params
--         url = "customers/" <> custId <> "/subscriptions"
--         params = [("plan", T.encodeUtf8 plan)]

-- getSubscription :: CustomerId -> SubscriptionId -> IO ()
-- getSubscription (CustomerId custId) (SubscriptionId subId) = sendStripeRequest req config
--   where req = StripeRequest GET url []
--         url = "customers/" <> custId <> "/subscriptions/" <> subId

-- -- see parameters on this one
-- updateSubscription :: CustomerId -> SubscriptionId -> IO ()
-- updateSubscription (CustomerId custId) (SubscriptionId subId) = sendStripeRequest req config
--   where req = StripeRequest POST url []
--         url = "customers/" <> custId <> "/subscriptions/" <> subId

-- deleteSubscription :: CustomerId -> SubscriptionId -> IO ()
-- deleteSubscription (CustomerId custId) (SubscriptionId subId) = sendStripeRequest req config
--   where req = StripeRequest DELETE url []
--         url = "customers/" <> custId <> "/subscriptions/" <> subId

-- -- Plans
-- newtype Plan = Plan Text deriving (Show, Eq)

-- newtype PlanId = PlanId Text deriving (Show, Eq)
-- type Amount = Int

-- check all the optional ones as well
-- createPlan (PlanId planId) amount = sendStripeRequest req config
--   where req = StripeRequest POST url params
--         url = "plans"
--         params = [ ("id", "basic") -- required
--                  , ("amount", "0") -- required
--                  , ("currency", "usd") -- required
--                  , ("interval", "month") -- required
--                  , ("name","Gold Special") -- required
--                  ]

-- getPlan (PlanId planId) = sendStripeRequest req config
--   where req = StripeRequest GET url []
--         url = "plans/" <> planId

-- -- optional :: name, metadata, statement_description
-- updatePlan (PlanId planId) = sendStripeRequest req config
--   where req = StripeRequest POST url []
--         url = "plans/" <> planId

-- deletePlan (PlanId planId) = sendStripeRequest req config
--   where req = StripeRequest DELETE url []
--         url = "plans/" <> planId

-- getPlans = sendStripeRequest req config
--   where req = StripeRequest GET url []
--         url = "plans"

-- ---------- Coupons

-- -- see optional
-- -- You must set either percent_off or amount_off and currency
-- createCoupon :: IO ()
-- createCoupon = sendStripeRequest req config
--   where req = StripeRequest POST url params
--         url = "coupons"
--         params = [ ("duration", "once")
--                  , ("percent_off", "25")
--                  ]

-- newtype CouponId = CouponId { couponId :: Text } deriving (Show, Eq)

-- getCoupon :: CouponId -> IO ()
-- getCoupon (CouponId couponId) = sendStripeRequest req config
--   where req = StripeRequest POST url []
--         url = "coupons/" <> couponId

-- deleteCoupon :: CouponId -> IO ()
-- deleteCoupon (CouponId couponId) = sendStripeRequest req config
--   where req = StripeRequest DELETE url []
--         url = "coupons/" <> couponId

-- getCoupons :: IO ()
-- getCoupons = sendStripeRequest req config
--   where req = StripeRequest GET url []
--         url = "coupons"

-- --------------- Discounts

-- deleteDiscount :: CustomerId -> IO ()
-- deleteDiscount (CustomerId customerId) = sendStripeRequest req config
--   where req = StripeRequest DELETE url []
--         url = "customers/" <> customerId <> "/discount"

-- deleteSubscriptionDiscount :: CustomerId -> SubscriptionId -> IO ()
-- deleteSubscriptionDiscount (CustomerId customerId) (SubscriptionId subId) =
--     sendStripeRequest req config
--   where req = StripeRequest DELETE url []
--         url = T.concat ["customers/"
--                        , customerId
--                        , "/subscriptions/"
--                        , subId
--                        , "/discount"
--                        ]

-- --- Invoices
-- -- https://stripe.com/docs/api#retrieve_invoiceitem
-- newtype InvoiceId = InvoiceId { invoiceId :: Text } deriving (Show, Eq)

-- -- unsure this one is correct
-- getInvoice :: InvoiceId -> IO ()
-- getInvoice (InvoiceId invoiceId) =
--     sendStripeRequest req config
--   where req = StripeRequest GET url []
--         url = "invoices/" <> invoiceId

-- -- see optional params
-- getInvoiceLineItems :: InvoiceId -> IO ()
-- getInvoiceLineItems (InvoiceId invoiceId) =
--     sendStripeRequest req config
--   where req = StripeRequest GET url []
--         url = T.concat ["invoices/"
--                        , invoiceId
--                        , "/lines"
--                        ]

-- createInvoice :: CustomerId -> IO ()
-- createInvoice (CustomerId customerId) =
--     sendStripeRequest req config
--   where req = StripeRequest POST "invoices" params
--         params = [ ("customer", T.encodeUtf8 customerId) ]

-- payInvoice :: InvoiceId -> IO ()
-- payInvoice (InvoiceId invoiceId) =
--     sendStripeRequest req config
--   where req = StripeRequest POST url []
--         url = "invoices/" <> invoiceId <> "/pay"

-- -- see optional params
-- updateInvoice :: InvoiceId -> IO ()
-- updateInvoice (InvoiceId invoiceId) =
--     sendStripeRequest req config
--   where req = StripeRequest POST url []
--         url = "invoices/" <> invoiceId

-- -- see optional for customer and date
-- getInvoices :: IO ()
-- getInvoices = sendStripeRequest req config
--   where req = StripeRequest GET url []
--         url = "invoicesadf"

-- -- see customer and subscription as optional
-- getUpcomingInvoice :: CustomerId -> IO ()
-- getUpcomingInvoice (CustomerId customerId) =
--     sendStripeRequest req config
--   where req = StripeRequest GET url []
--         url = "invoices/upcoming?customer=" <> customerId

-- ---- Create Invoice Items
-- createInvoiceItem :: CustomerId -> IO ()
-- createInvoiceItem (CustomerId customerId) =
--     sendStripeRequest req config
--   where req = StripeRequest POST "invoiceitems" params
--         params = [ ("customer", T.encodeUtf8 customerId)
--                  , ("amount", "1000")
--                  , ("currency", "usd")
--                  ]

-- newtype InvoiceItemId = InvoiceItemId Text deriving (Eq, Show)

-- getInvoiceItem :: InvoiceItemId -> IO ()
-- getInvoiceItem (InvoiceItemId itemId) =
--     sendStripeRequest req config
--   where req = StripeRequest GET url []
--         url = "invoiceitems/" <> itemId

-- -- see additional parameters
-- updateInvoiceItem :: InvoiceItemId -> IO ()
-- updateInvoiceItem (InvoiceItemId itemId) =
--     sendStripeRequest req config
--   where req = StripeRequest POST url []
--         url = "invoiceitems/" <> itemId

-- deleteInvoiceItem :: InvoiceItemId -> IO ()
-- deleteInvoiceItem (InvoiceItemId itemId) =
--     sendStripeRequest req config
--   where req = StripeRequest DELETE url []
--         url = "invoiceitems/" <> itemId

-- getInvoiceItems :: IO ()
-- getInvoiceItems = sendStripeRequest req config
--   where req = StripeRequest GET "invoiceitems" []

-- --- Disputes
-- -- see additional params
-- updateDispute :: Charge -> IO ()
-- updateDispute (Charge chargeId) = sendStripeRequest req config
--   where req = StripeRequest POST url []
--         url = "charges/" <> chargeId <> "/dispute"

-- -- test this
-- closeDispute :: Charge -> IO ()
-- closeDispute (Charge chargeId) = sendStripeRequest req config
--   where req = StripeRequest POST url []
--         url = "charges/" <> chargeId <> "/dispute/close"

-- ---- transfers
-- -- amount = required
-- -- currency = required
-- -- recipient = required
-- -- see optional
-- newtype RecipientId = RecipientId { recipientId :: Text } deriving (Show, Eq)
-- newtype TransferId = TransferId { transferId :: Text } deriving (Show, Eq)

-- createTransfer :: RecipientId -> IO ()
-- createTransfer (RecipientId recipientId) = sendStripeRequest req config
--   where req = StripeRequest POST "transfers" params
--         params = [ ("amount", "400")
--                  , ("currency", "usd")
--                  , ("recipient", T.encodeUtf8 recipientId)
--                  ]

-- getTransfer :: TransferId -> IO ()
-- getTransfer (TransferId transferId) = sendStripeRequest req config
--   where req = StripeRequest GET url []
--         url = "transfers/" <> transferId

-- -- see additional description and metadata
-- updateTransfer :: TransferId -> IO ()
-- updateTransfer (TransferId transferId) = sendStripeRequest req config
--   where req = StripeRequest POST url []
--         url = "transfers/" <> transferId

-- cancelTransfer :: TransferId -> IO ()
-- cancelTransfer (TransferId transferId) = sendStripeRequest req config
--   where req = StripeRequest POST url []
--         url = "transfers/" <> transferId <> "/cancel"

-- -- see optional params
-- getTransfers :: IO ()
-- getTransfers = sendStripeRequest req config
--   where req = StripeRequest GET url []
--         url = "transfers"

-- ---- Recipients
-- data RecipientType = Individual | Corporation deriving (Eq, Show)
-- type Name = Text

-- createRecipient :: Name -> RecipientType -> IO ()
-- createRecipient name recipientType  = sendStripeRequest req config
--   where req = StripeRequest POST "recipients" params
--         params = [ ("name", T.encodeUtf8 name)
--                  , ("type", if recipientType == Individual
--                             then "individual"
--                             else "corporation")
--                  ]

-- getRecipient :: RecipientId -> IO ()
-- getRecipient (RecipientId recipientId) = sendStripeRequest req config
--   where req =  StripeRequest GET url []
--         url = "recipients/" <> recipientId

-- -- see optional
-- updateRecipient :: RecipientId -> IO ()
-- updateRecipient (RecipientId recipientId) = sendStripeRequest req config
--   where req =  StripeRequest POST url []
--         url = "recipients/" <> recipientId

-- deleteRecipient :: RecipientId -> IO ()
-- deleteRecipient (RecipientId recipientId) = sendStripeRequest req config
--   where req =  StripeRequest DELETE url []
--         url = "recipients/" <> recipientId

-- -- get all recipients
-- getRecipients :: IO ()
-- getRecipients = sendStripeRequest req config
--   where req =  StripeRequest GET "recipients" []

-- -------------- application fees
-- newtype FeeId = FeeId { feeId :: Text } deriving (Show, Eq)

-- getApplicationFee :: FeeId -> IO ()
-- getApplicationFee (FeeId feeId) = sendStripeRequest req config
--   where req =  StripeRequest GET url []
--         url = "application_fees/" <> feeId

-- refundApplicationFee :: FeeId -> IO ()
-- refundApplicationFee (FeeId feeId) = sendStripeRequest req config
--   where req =  StripeRequest POST url []
--         url = "application_fees/" <> feeId <> "/refund"

-- -- see optional
-- getApplicationFees :: FeeId -> IO ()
-- getApplicationFees (FeeId feeId) = sendStripeRequest req config
--   where req =  StripeRequest GET "application_fees"  []

-- ------------ Application Details
-- getAccountDetails :: IO ()
-- getAccountDetails = sendStripeRequest req config
--   where req =  StripeRequest GET "account"  []

-- getAccountBalance :: IO ()
-- getAccountBalance = sendStripeRequest req config
--   where req =  StripeRequest GET "balance"  []

-- newtype TransactionId = TransactionId { transactionId :: Text } deriving (Show, Eq)

-- getBalanceTransaction :: TransactionId -> IO ()
-- getBalanceTransaction (TransactionId transactionId) =
--     sendStripeRequest req config
--   where req =  StripeRequest GET url []
--         url = "balance/history/" <> transactionId

-- -- see optional
-- getBalanceHistory :: IO ()
-- getBalanceHistory = sendStripeRequest req config
--   where req = StripeRequest GET url []
--         url = "balance/history"

-- ---- Retrieve an event
-- newtype EventId = EventId { eventId :: Text } deriving (Show, Eq)

-- data EventType = ChargeEvent
--    | RefundEvent
--    | AdjustmentEvent
--    | ApplicationFeeEvent
--    | ApplicationFeeRefundEvent
--    | TransferEvent
--    | TransferFailureEvent
--      deriving (Show, Eq)

-- getEvent :: EventId -> IO ()
-- getEvent (EventId evtId) = sendStripeRequest req config
--   where req = StripeRequest GET url []
--         url = "events/" <> evtId

-- -- see optional
-- getEvents :: IO ()
-- getEvents = sendStripeRequest req config
--   where req = StripeRequest GET "events" []

-- --- token
-- newtype Token = Token Text deriving (Show, Eq, Ord)

-- createCardToken :: IO ()
-- createCardToken = sendStripeRequest req config
--   where req = StripeRequest POST url params
--         url = "tokens"
--         params = [ ("card[number]", "4242424242424242")
--                  , ("card[exp_month]", "12")
--                  , ("card[exp_year]", "2015")
--                  , ("card[cvc]", "123")
--                  ]

-- createBankAccountToken :: IO ()
-- createBankAccountToken = sendStripeRequest req config
--   where req = StripeRequest POST url params
--         url = "tokens"
--         params = [ ("bank_account[country]", "US")
--                  , ("bank_account[routing_number]", "110000000")
--                  , ("bank_account[account_number]", "000123456789")
--                  ]

-- getToken :: Token -> IO ()
-- getToken (Token token) = sendStripeRequest req config
--   where req = StripeRequest GET url params
--         url = "tokens/" <> token
--         params = []
---








