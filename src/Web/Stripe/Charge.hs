{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Web.Stripe.Charge where

import           Control.Applicative
import           Data.Aeson
import           Data.Monoid
import           Data.Text                       (Text)
import qualified Data.Text.Encoding              as T
import           Network.Http.Client
import           Web.Stripe.Client
import           Web.Stripe.Internal.Class
import           Web.Stripe.Internal.StripeError
import           Web.Stripe.Util

-- charges, lots of optional fields as well
newtype Charge = Charge Text deriving (Show, Eq)
newtype RefundId = RefundId { refundId :: Text } deriving (Show, Eq)

config = StripeConfig "sk_test_zvqdM2SSA6WwySqM6KJQrqpH" "2014-03-28"

defaultChargeOptions = 
    ChargeOptions Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing 

-- either customer Id or charge is required
data Charge = Charge {
      chargeId                   :: Text
    , chargeObject               :: Text
    , chargeCreated              :: UTCTime
    , chargeLiveMode             :: Bool
    , chargePaid                 :: Bool
    , chargeAmount               :: Int
    , chargeCurrency             :: Maybe Text
    , chargeCustomerId           :: Maybe Text
    , chargeCardId               :: Maybe Text
    , chargeDescription          :: Maybe Text
    , chargeCapture              :: Maybe Text
    , chargeStatementDescription :: Maybe Text
    , chargeReceiptEmail         :: Maybe Text
    } deriving (Show, Eq)

instance FromJSON Charge where
    parseJSON (Object o) = 
        do Charge <$> o .: "id"
                  <*> o .: "object"
                  <*> o .: "created"
                  <*> o .: "livemode"
                  <*> o .: "paid"
                  <*> o .: "amount"

                      
                           

-- data GetChargeOptions = GetChargeOptions {
--       chargeId               :: Int
--     , chargeAmount           :: Maybe Int
--     , chargeStatementDescription :: Maybe Text
--     , chargeReceiptEmail         :: Maybe Text
--     } deriving (Show, Eq)

instance URLEncodeable () where
    formEncode () = []

-- instance URLEncodeable ChargeOptions where
--     formEncode ChargeOptions{..} =
--         [ (a, b) | (a, Just b) <- [
--            ("id", fmap toBS chargeId)
--          , ("amount", fmap toBS chargeAmount)
--          , ("currency", fmap toBS chargeDescription)
--          , ("customer", fmap toBS chargeCustomerId)
--          , ("card", fmap toBS chargeCardId)
--          , ("description", fmap toBS chargeDescription)
--          , ("capture", fmap toBS chargeCapture)
--          , ("statement_description", fmap toBS chargeStatementDescription)
--          , ("receipt_email", fmap toBS chargeReceiptEmail)
--          ]
--         ]

-- charge :: Either Card CustomerId -> IO ()
-- charge param = sendStripeRequest req config
--   where req = StripeRequest POST "charges" params
--         params = result : [ ("amount", "400")
--                           , ("currency", "usd")
--                           ]
--         result =
--             case param of
--               Right (CustomerId custId) ->
--                   ("customer", T.encodeUtf8 custId)
--               Left (Card cardId) ->
--                   ("card", T.encodeUtf8 cardId)

-- chargeByCardId :: Card -> IO ()
-- chargeByCardId card = charge $ Left card

-- chargeByCustomerId :: CustomerId -> IO ()
-- chargeByCustomerId customerId = charge $ Right customerId

getCharge :: FromJSON a => Charge -> IO (Either StripeError a)
getCharge (Charge charge) = sendStripeRequest config req ()
  where req = StripeRequest GET url 
        url = "charges/" <> charge



-- -- description + metadata == options
-- updateCharge :: Charge -> IO ()
-- updateCharge (Charge charge) = sendStripeRequest req config
--   where req = StripeRequest POST url []
--         url = "charges/" <> charge

-- -- optional params

data Charge = Charge {

}

captureCharge :: FromJSON a => Charge -> IO (Either StripeError a)
captureCharge (Charge charge) = sendStripeRequest config req ()
  where req = StripeRequest POST url
        url = "charges/" <> charge <> "/capture"

-- -- optional params
-- getCharges :: FromJSON a => IO (Either StripeError a)
-- getCharges = sendStripeRequest config req chargeOptions
--   where req = StripeRequest GET "charges" []
-- --


