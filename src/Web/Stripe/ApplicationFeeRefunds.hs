module Web.Stripe.ApplicationFeeRefunds where

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