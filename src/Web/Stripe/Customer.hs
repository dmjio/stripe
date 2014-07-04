{-# LANGUAGE RecordWildCards #-}

module Web.Stripe.Customer
    ( -- * Types
      Customer(..)
    , CustomerId(..)
      -- * API Calls
    , createCustomer
    , updateCustomer
    , deleteCustomer
    , getCustomer
    ) where

import           Control.Applicative
import           Data.Aeson
import           Data.Monoid                     ((<>))
import           Data.Text                       (Text)
import           Data.Time.Clock
import           Network.Http.Client
import           Web.Stripe.Client

import           Web.Stripe.Internal.Class
import           Web.Stripe.Internal.StripeError
import           Web.Stripe.Util

config :: StripeConfig
config = StripeConfig "sk_test_zvqdM2SSA6WwySqM6KJQrqpH" "2014-03-28"

-- https://stripe.com/docs/api#customers
-------------------------------------------------------------
newtype CustomerId = CustomerId Text deriving (Show)

data Customer = Customer {
      customerCreated        :: UTCTime
    , customerId             :: Text
    , customerLiveMode       :: Bool
    , delinquent             :: Bool
    , customerDescription    :: Maybe Text
    , customerEmail          :: Maybe Text
    , customerMetaData       :: Maybe Object
    , customerAccountBalance :: Maybe Int
    , customerCurrency       :: Maybe Text
    , customerDiscount       :: Maybe Discount
    } | DeletedCustomer {
      isDeleted  :: Bool
    , deletionId :: Text
    } deriving (Show, Eq)

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

type Balance = Int
type Quantity = Int

data CustomerOptions = CustomerOptions {
      customerAccountBalanceOptions :: Maybe Balance
--  , customerCard :: Card
--  , customerCoupon :: Maybe Coupon
    , customerDescriptonOptions     :: Maybe Text
    , customerEmailOptions          :: Maybe Text
--  , customerMetaData :: Maybe Text
--  , customerPlan :: Maybe Plan
    , customerQuantityOptions       :: Maybe Int
    , customerTrialEndOptions       :: Maybe Int
}

-------------------------------------------------------------
defaultCustomerOptions :: CustomerOptions
defaultCustomerOptions =
    CustomerOptions Nothing Nothing Nothing Nothing  Nothing

instance URLDecodeable CustomerOptions where
    formEncode CustomerOptions{..} =
        [ (a, b) | (a, Just b) <- [
           ("account_balance", fmap toBS customerAccountBalanceOptions)
         , ("description", fmap toBS customerDescriptonOptions)
         , ("email", fmap toBS customerEmailOptions)
         , ("quantity", fmap toBS customerQuantityOptions)
         , ("trial_end", fmap toBS customerTrialEndOptions)
         ]
        ]

instance URLDecodeable () where
    formEncode ()   = []

createDefaultCustomer :: IO (Either StripeError Customer)
createDefaultCustomer = createCustomer defaultCustomerOptions

createCustomer :: CustomerOptions -> IO (Either StripeError Customer)
createCustomer options = sendStripeRequest config req options
  where req = StripeRequest POST "customers"

getCustomer :: CustomerId -> IO (Either StripeError Customer)
getCustomer (CustomerId cid) = sendStripeRequest config req ()
  where req = StripeRequest GET url
        url = "customers/" <> cid

updateCustomer :: CustomerOptions -> CustomerId -> IO (Either StripeError Customer)
updateCustomer options (CustomerId cid) = sendStripeRequest config req options
  where req = StripeRequest POST url
        url = "customers/" <> cid

deleteCustomer :: CustomerOptions -> CustomerId ->  IO (Either StripeError Customer)
deleteCustomer options (CustomerId cid) = sendStripeRequest config req options
  where req = StripeRequest DELETE url
        url = "customers/" <> cid

-------------------------------------------------------------

instance FromJSON Customer where
    parseJSON (Object o)
        = Customer
           <$> (fromSeconds <$> o .: "created")
           <*> o .: "id"
           <*> o .: "livemode"
           <*> o .: "delinquent"
           <*> o .:? "description"
           <*> o .:? "email"
           <*> o .:? "metadata"
           <*> o .:? "account_balance"
           <*> o .:? "currency"
           <*> o .:? "discount"
      <|> DeletedCustomer
           <$> o .: "deleted"
           <*> o .: "id"

