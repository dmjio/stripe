module Web.Stripe.ApplicationFee
    ( export
    , export
    ) where

import           Control.Applicative             ((<$>), (<*>))
import           Data.Aeson
import           Data.Monoid
import           Data.Text                       (Text)
import           Data.Time
import           Web.Stripe.Client.Internal
import           Web.Stripe.Util
import           Web.Stripe.Refunds
import           Web.Stripe.Internal.StripeError

data ApplicationFee = ApplicationFee {
      applicationFeeId :: Text
    , applicationFeeCreated :: UTCTime
    , applicationFeeLiveMode :: Bool
    , applicationFeeAmount :: Int
    , applicationFeeCurrency :: Text
    , applicationFeeRefunded :: Bool
    , applicationFeeAmountRefunded :: Int
    , applicationFeeRefunds :: StripeList Refunds
} deriving (Show)

instance FromJSON ApplicationFee where
   parseJSON (Object o) = undefined

newtype FeeId = FeeId { feeId :: Text } deriving (Show, Eq)

getApplicationFee :: FeeId -> IO (Either StripeError ApplicationFee)
getApplicationFee (FeeId feeId) = callAPI config req []
  where req =  StripeRequest GET url 
        url = "application_fees/" <> feeId

refundApplicationFee :: FeeId -> IO (Either StripeError ApplicationFee)
refundApplicationFee (FeeId feeId) = callAPI config req  []
  where req =  StripeRequest POST url 
        url = "application_fees/" <> feeId <> "/refund"

-- see optional
getApplicationFees :: FeeId -> IO (Either StripeError (StripeList ApplicationFee))
getApplicationFees (FeeId feeId) = callAPI config req []
  where req =  StripeRequest GET "application_fees"  

