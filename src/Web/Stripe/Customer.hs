module Web.Stripe.Customer
    ( -- * Types
      Customer(..)
    , CustomerId(..)
      -- * API Calls
    , createCustomer
    , updateCustomer
    , deleteCustomer
    , getCustomers
    , getCustomer
    ) where

import           Control.Applicative 
import           Data.Aeson
import           Data.Monoid ((<>))
import           Data.Text    (Text)
import           Web.Stripe.Client
import           Network.Http.Client 
import           Data.Time.Clock

import           Web.Stripe.Util
import           Web.Stripe.Internal.StripeError
-- import           Web.Stripe.Card

config :: StripeConfig
config = StripeConfig "sk_test_zvqdM2SSA6WwySqM6KJQrqpH" "2014-03-28"
  
-- https://stripe.com/docs/api#customers
-------------------------------------------------------------
newtype CustomerId = CustomerId Text deriving (Show)

data Customer = Customer {
      customerCreated :: UTCTime
    , customerId :: Text
    , customerLiveMode :: Bool
    , delinquent :: Bool
    , customerDescription :: Maybe Text
    , customerEmail :: Maybe Text
    , customerMetaData :: Maybe Object
    } deriving (Show, Eq)

-------------------------------------------------------------
createCustomer :: IO (Either StripeError Customer)
createCustomer = sendStripeRequest req config
  where req = StripeRequest POST "customers" []

getCustomer :: CustomerId -> IO (Either StripeError Customer)
getCustomer (CustomerId cid) = sendStripeRequest req config
  where req = StripeRequest GET url []
        url = "customers/" <> cid 

updateCustomer :: CustomerId -> IO (Either StripeError Customer)
updateCustomer (CustomerId cid) = sendStripeRequest req config
  where req = StripeRequest POST url []
        url = "customers/" <> cid 

deleteCustomer :: CustomerId ->  IO (Either StripeError StripeDeletionResult) 
deleteCustomer (CustomerId cid) = sendStripeRequest req config
  where req = StripeRequest DELETE url []
        url = "customers/" <> cid 

getCustomers ::  IO (Either StripeError Customer)
getCustomers = sendStripeRequest req config
  where req = StripeRequest GET url []
        url = "customers"

-------------------------------------------------------------
data StripeDeletionResult = StripeDeletionResult { 
      isDeleted :: Bool
    , deletionId :: Text 
    } deriving (Show, Eq)

instance FromJSON StripeDeletionResult where
    parseJSON (Object o) = 
      StripeDeletionResult <$> o .: "deleted" 
                           <*> o .: "id"

instance FromJSON Customer where
    parseJSON (Object o) = 
        Customer <$> (fromSeconds <$> o .: "created")
                 <*> o .: "id" 
                 <*> o .: "livemode" 
                 <*> o .: "delinquent"
                 <*> o .:? "description" 
                 <*> o .:? "email"
                 <*> o .:? "metadata"
