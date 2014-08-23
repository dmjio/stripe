module Web.Stripe.ApplicationFeeRefunds where

import Data.Text
import Web.Stripe.Types

config :: StripeConfig
config = StripeConfig "sk_test_zvqdM2SSA6WwySqM6KJQrqpHss" "2014-03-28"

-- Application Fee Refund
newtype ApplicationFeeRefundId = ApplicationFeeRefundId Text deriving (Show, Eq)

data ApplicationFeeRefund = ApplicationFeeRefund {  
       applicationFeeRefundId :: ApplicationFeeRefundId
     , applicationFeeRefundAmount :: Int
     , applicationFeeRefundCreated :: UTCTime
     , applicationFeeRefundCurrency :: Text
     , applicationFeeRefundBalanceTransaction :: Maybe Text
     , applicationFeeRefundFee :: FeeId
     } deriving Show

instance FromJSON ApplicationFeeRefund where
    parseJSON (Object o) = undefined  	