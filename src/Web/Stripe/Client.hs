{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Web.Stripe.Client where

import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString            as S
import qualified Data.ByteString.Lazy       as BL
import Data.Time.Clock
import qualified Data.ByteString.Lazy.Char8 as BL8
import Text.Printf
import qualified Data.Text.Encoding as T
import qualified Data.Text as T
import           Data.HashMap.Strict        as H
import           Data.Monoid                ((<>))
import           Data.Text                  (Text)
import           Data.Typeable
import           Network.Http.Client
import           OpenSSL                    (withOpenSSL)
import Data.Maybe (mapMaybe)
import qualified System.IO.Streams          as Streams

type URL = S.ByteString
type RequestParams = [(S.ByteString, S.ByteString)]

data StripeRequest = StripeRequest
    { method :: Method
    , url    :: Text
    , params :: RequestParams
    } deriving (Show)

data StripeConfig = StripeConfig
    { secretKey  :: S.ByteString
    , apiVersion :: S.ByteString
    } deriving (Show)

data StripeErrorType = InvalidRequest 

data StripeError = StripeError {
      errorType :: Text
    , errorMsg :: Text
    } deriving (Show)

-- RAW JSON
-- "{\n  \"error\": {\n    \"type\": \"invalid_request_error\",\n    \"message\": \"Unrecognized request URL (GET: /v1/customersasdfkljsadf).  Please see https://stripe.com/docs or we can help at https://s\
-- upport.stripe.com/.\"\n  }\n}\n"

-- PARSER
instance FromJSON StripeError where -- fails w/ Nothing
    parseJSON (Object err) = 
        do String typ <- err .: "type"
           String msg <- err .: "message"
           return $ StripeError typ msg

-- How it looks as (decode x  :: Maybe Object)
-- Just fromList [("error",Object fromList [("type",String "invalid_request_error"),("message",String "Unrecognized request URL (GET: /v1/customersasdfkljsadf).  Please see https://stripe.com/docs or we ca\
-- n help at https://support.stripe.com/.")])]

sendStripeRequest :: StripeRequest -> StripeConfig -> IO ()
sendStripeRequest StripeRequest{..} StripeConfig{..} = withOpenSSL $ do
  ctx <- baselineContextSSL
  c <- openConnectionSSL ctx "api.stripe.com" 443
  q <- buildRequest $ do
          http method $ "/v1/" <> T.encodeUtf8 url
          setAuthorizationBasic secretKey ""
          setContentType "application/x-www-form-urlencoded"
          setHeader "Stripe-Version" apiVersion
  body <- Streams.fromByteString $ convertToString params
  sendRequest c q (inputStreamBody body)
  res <- receiveResponse c $ \p i -> do
         xm <- Streams.read i
         print $ ("code", getStatusCode p)
         print $ ("msg", getStatusMessage p)
         case xm of
             Just x  -> print (decodeStrict x :: Maybe Object)
             Nothing -> return ()
  print res
  closeConnection c
  return res

convertToString :: RequestParams -> S.ByteString
convertToString [] = ""
convertToString ((x,y) : []) = x <> "=" <> y
convertToString ((x,y) : xs) = x <> "=" <> y <> "&" <> convertToString xs

config = StripeConfig "sk_test_zvqdM2SSA6WwySqM6KJQrqpH" "2014-03-28"

-- util
toParams :: [(a, Maybe b)] -> [(a, b)]
toParams = mapMaybe . uncurry $ fmap . (,)
--

-- charges, lots of optional fields as well
newtype Charge = Charge Text deriving (Show, Eq)

charge :: Either Card CustomerId -> IO ()
charge param = sendStripeRequest req config
  where req = StripeRequest POST "charges" params
        params = result : [ ("amount", "400")
                          , ("currency", "usd")
                          ] 
        result = case param of
                   Right (CustomerId custId) -> 
                       ("customer", T.encodeUtf8 custId)
                   Left (Card cardId) -> 
                       ("card", T.encodeUtf8 cardId)

chargeByCardId :: Card -> IO ()
chargeByCardId card = charge $ Left card

chargeByCustomerId :: CustomerId -> IO ()
chargeByCustomerId customerId = charge $ Right customerId

getCharge :: Charge -> IO ()
getCharge (Charge charge) = sendStripeRequest req config
  where req = StripeRequest GET url []
        url = "charges/" <> charge

-- description + metadata == options
updateCharge :: Charge -> IO ()
updateCharge (Charge charge) = sendStripeRequest req config
  where req = StripeRequest POST url []
        url = "charges/" <> charge

-- optional params
captureCharge :: Charge -> IO ()
captureCharge (Charge charge) = sendStripeRequest req config
  where req = StripeRequest POST url []
        url = "charges/" <> charge <> "/capture"

-- optional params
getCharges :: IO ()
getCharges = sendStripeRequest req config
  where req = StripeRequest GET "charges" []
--

-- create a refund
createRefund :: Charge -> IO ()
createRefund (Charge chargeId) = sendStripeRequest req config
  where req = StripeRequest POST url []
        url = "charges/" <> chargeId <> "/refunds"

newtype RefundId = RefundId { refundId :: Text } deriving (Show, Eq)
getRefund :: Charge -> RefundId -> IO ()
getRefund (Charge chargeId) (RefundId refId) = sendStripeRequest req config
  where req = StripeRequest GET url []
        url = "charges/" <> chargeId <> "/refunds/" <> refId

-- optional metadata
updateRefund :: Charge -> RefundId -> IO ()
updateRefund (Charge chargeId) (RefundId refId) = sendStripeRequest req config
  where req = StripeRequest POST url []
        url = "charges/" <> chargeId <> "/refunds/" <> refId
---


---- Customer
newtype Email = Email Text
newtype CustomerId = CustomerId Text
-- data Customer = Customer
--     { custId          :: CustomerId
--     , custEmail       :: Maybe Email
--     , custDescription :: Maybe Description
--     , custLive        :: Bool
--     , custCreated     :: UTCTime
--     , custActiveCard  :: Maybe Card
--     , custDiscount    :: Maybe Discount
--     } deriving Show

-- | optional creation parameters
-- * account_balance=0
-- * card=tok_4JWC6MoT5gbS0f
-- * coupon=couponCode123
-- * description=couponCode123
-- * email=couponCode123
-- * metadata=--set of k/v pairs for customer
-- * plan=planthings
-- * quantity=10
-- * trial_end=131244
-- 

createCustomer :: IO ()
createCustomer = sendStripeRequest req config
  where req = StripeRequest POST "customers" []

getCustomer :: CustomerId -> IO ()
getCustomer (CustomerId cid) = sendStripeRequest req config
  where req = StripeRequest GET url []
        url = "customers/" <> cid 

-- | optional udpate parameters
-- * account_balance=0
-- * card=tok_4JWC6MoT5gbS0f
-- * coupon=couponCode123
-- * description=couponCode123
-- * email=couponCode123
-- * metadata=--set of k/v pairs for customer

updateCustomer :: CustomerId -> IO ()
updateCustomer (CustomerId cid) = sendStripeRequest req config
  where req = StripeRequest POST url []
        url = "customers/" <> cid 

deleteCustomer :: CustomerId -> IO ()
deleteCustomer (CustomerId cid) = sendStripeRequest req config
  where req = StripeRequest DELETE url []
        url = "customers/" <> cid 

getCustomers :: IO ()
getCustomers = sendStripeRequest req config
  where req = StripeRequest GET url []
        url = "customers"
----

-- create a token
newtype Token = Token Text deriving (Show, Eq, Ord)

createCardToken :: IO ()
createCardToken = sendStripeRequest req config
  where req = StripeRequest POST url params
        url = "tokens" 
        params = [ ("card[number]", "4242424242424242")
                 , ("card[exp_month]", "12")
                 , ("card[exp_year]", "2015")
                 , ("card[cvc]", "123")
                 ]

createBankAccountToken :: IO ()
createBankAccountToken = sendStripeRequest req config
  where req = StripeRequest POST url params
        url = "tokens" 
        params = [ ("bank_account[country]", "US")
                 , ("bank_account[routing_number]", "110000000")
                 , ("bank_account[account_number]", "000123456789")
                 ]

getToken :: Token -> IO ()
getToken (Token token) = sendStripeRequest req config
  where req = StripeRequest GET url params
        url = "tokens/" <> token
        params = []
--- 

-- Create card
newtype Card = Card { cardId :: Text } deriving (Show, Eq)

createCard :: CustomerId -> Token -> IO ()
createCard (CustomerId cid) (Token token_id) = sendStripeRequest req config
  where req = StripeRequest POST url []
        url = "customers/" <> cid 
        params = [("card", token_id)] -- card is required

-- See all the optional arguments here, lots of them
updateCard :: CustomerId -> Card -> IO ()
updateCard (CustomerId custId) (Card cardId) = sendStripeRequest req config
  where req = StripeRequest DELETE url []
        url = "customers/" <> custId <> "/cards/" <> cardId

deleteCard :: CustomerId -> Card -> IO ()
deleteCard (CustomerId custId) (Card cardId) = sendStripeRequest req config
  where req = StripeRequest DELETE url []
        url = "customers/" <> custId <> "/cards/" <> cardId

-- optional args
-- ending_before
-- limit
-- starting_after
getCards :: CustomerId -> IO ()
getCards (CustomerId custId) = sendStripeRequest req config
  where req = StripeRequest GET url []
        url = "customers/" <> custId <> "/cards"

--- Subscriptions
newtype SubscriptionId = SubscriptionId { subscriptionId :: Text } deriving (Show, Eq)
createSubscription :: CustomerId -> PlanId -> IO ()
createSubscription (CustomerId custId) (PlanId plan) = sendStripeRequest req config
  where req = StripeRequest POST url params
        url = "customers/" <> custId <> "/subscriptions"
        params = [("plan", T.encodeUtf8 plan)]

getSubscription :: CustomerId -> SubscriptionId -> IO ()
getSubscription (CustomerId custId) (SubscriptionId subId) = sendStripeRequest req config
  where req = StripeRequest GET url []
        url = "customers/" <> custId <> "/subscriptions/" <> subId

-- see parameters on this one
updateSubscription :: CustomerId -> SubscriptionId -> IO ()
updateSubscription (CustomerId custId) (SubscriptionId subId) = sendStripeRequest req config
  where req = StripeRequest POST url []
        url = "customers/" <> custId <> "/subscriptions/" <> subId

deleteSubscription :: CustomerId -> SubscriptionId -> IO ()
deleteSubscription (CustomerId custId) (SubscriptionId subId) = sendStripeRequest req config
  where req = StripeRequest DELETE url []
        url = "customers/" <> custId <> "/subscriptions/" <> subId

---





-- Plans
-- newtype Plan = Plan Text deriving (Show, Eq)

newtype PlanId = PlanId Text deriving (Show, Eq)
type Amount = Int

-- check all the optional ones as well
createPlan :: PlanId -> Amount -> IO ()
createPlan (PlanId planId) amount = sendStripeRequest req config
  where req = StripeRequest POST url params
        url = "plans" 
        params = [ ("id", "basic") -- required
                 , ("amount", "0") -- required
                 , ("currency", "usd") -- required
                 , ("interval", "month") -- required
                 , ("name","Gold Special") -- required
                 ]

getPlan :: PlanId -> IO () 
getPlan (PlanId planId) = sendStripeRequest req config
  where req = StripeRequest GET url []
        url = "plans/" <> planId

-- optional :: name, metadata, statement_description
updatePlan :: PlanId -> IO () 
updatePlan (PlanId planId) = sendStripeRequest req config
  where req = StripeRequest POST url []
        url = "plans/" <> planId
  
deletePlan :: PlanId -> IO () 
deletePlan (PlanId planId) = sendStripeRequest req config
  where req = StripeRequest DELETE url []
        url = "plans/" <> planId

getPlans :: IO () 
getPlans = sendStripeRequest req config
  where req = StripeRequest GET url []
        url = "plans"

--








makeBadRequest :: IO ()
makeBadRequest = sendStripeRequest req config
  where req = StripeRequest GET "customersasdfkljsadf" [("expand[]", "customer")]


