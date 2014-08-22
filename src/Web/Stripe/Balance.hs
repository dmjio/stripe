{-# LANGUAGE OverloadedStrings #-}
module Web.Stripe.Balance
    (
    ) where

import           Control.Applicative             ((<$>), (<*>))
import           Data.Aeson
import           Data.Monoid
import           Data.Text                       (Text)
import           Data.Time
import           Web.Stripe.Client.Internal
import           Web.Stripe.Util
import           Web.Stripe.Internal.StripeError

config :: StripeConfig
config = StripeConfig "sk_test_zvqdM2SSA6WwySqM6KJQrqpH" "2014-03-28"

-- ================= API Calls ===================== -- 
getBalance :: Stripe Balance
getBalance config = callAPI req 
  where req    = StripeRequest GET url params
        url    = "balance"
        params = []

getBalanceHistory :: Stripe (StripeList BalanceTransaction)
getBalanceHistory config = callAPI request 
  where request = StripeRequest GET url params
        url     = "balance/history"
        params  = []

getBalanceTransaction ::
    TransactionId ->
    IO (Either StripeError Balance)
getBalanceTransaction (TransactionId tId)  =
    callAPI request 
  where request = StripeRequest GET url params
        url     = "balance/history/" <> tId
        params  = []

-- ================= Types ===================== -- 
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

-- ================= JSON  ===================== -- 
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

                              
