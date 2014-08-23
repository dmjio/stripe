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
import           Web.Stripe.Card
import           Web.Stripe.Client.Internal
import           Web.Stripe.Coupon
import           Web.Stripe.Internal.StripeError
import           Web.Stripe.Plan
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
    , customerCard                  :: Card
    , customerCoupon                :: Maybe Coupon
    , customerDescriptonOptions     :: Maybe Text
    , customerEmailOptions          :: Maybe Text
    , customerPlan                  :: Maybe Plan
    , customerQuantityOptions       :: Maybe Int
    , customerTrialEndOptions       :: Maybe Int
}


createCustomer :: Stripe Customer
createCustomer = callAPI req
  where req = StripeRequest POST "customers" params
        params = [ (x, y) | (x, Just y) <- [
                     ("account_balance", fmap toBS customerAccountBalanceOptions)
                   , ("description", fmap toBS customerDescriptonOptions)
                   , ("email", fmap toBS customerEmailOptions)
                   , ("quantity", fmap toBS customerQuantityOptions)
                   , ("trial_end", fmap toBS customerTrialEndOptions)
                   ]
                 ]

getCustomer :: CustomerId -> Stripe Customer
getCustomer (CustomerId cid) = callAPI req ()
  where req = StripeRequest GET url
        url = "customers/" <> cid

updateCustomer :: CustomerOptions -> CustomerId -> Stripe Customer
updateCustomer options (CustomerId cid) = callAPI req options
  where req = StripeRequest POST url
        url = "customers/" <> cid

deleteCustomer :: CustomerOptions -> CustomerId ->  Stripe Customer
deleteCustomer options (CustomerId cid) = callAPI req options
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


