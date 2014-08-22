module Web.Stripe.Refunds 
    ( createRefund
    , getRefund
    , updateRefund
    , getRefunds
    ) where

import           Control.Applicative             ((<$>), (<*>))
import           Data.Aeson
import           Data.Monoid
import           Data.Text                       (Text)
import           Data.Time
import           Web.Stripe.Client.Internal
import           Web.Stripe.Util
import           Web.Stripe.Charge
import           Web.Stripe.Internal.StripeError

createRefund :: ChargeId -> Stripe Refund
createRefund (ChargeId chargeId) = callAPI req 
  where req = StripeRequest POST url []
        url = "charges/" <> chargeId <> "/refunds"

getRefund :: ChargeId -> RefundId -> Stripe Refund
getRefund (ChargeId chargeId) (RefundId refId) = callAPI req 
   where req = StripeRequest GET url params
         url = "charges/" <> chargeId <> "/refunds/" <> refId
         params = []

updateRefund :: ChargeId -> RefundId -> Stripe Refund
updateRefund (ChargeId chargeId) (RefundId refId) = callAPI req 
  where req    = StripeRequest POST url params
        url    = "charges/" <> chargeId <> "/refunds/" <> refId
        params = []

getRefunds :: ChargeId -> Stripe Refunds
getRefunds (ChargeId chargeId) = callAPI req 
  where req    = StripeRequest GET url params
        url    = "charges/" <> chargeId <> "/refunds"
        params = []

type Refunds = StripeList Refund
newtype ChargeId = ChargeId Text deriving (Show, Eq)
newtype RefundId = RefundId Text deriving (Show, Eq)

data Refund = Refund {
      refundId :: Text
    , refundAmount :: Int
    , refundCurrency :: Text
    , refundCreated :: UTCTime
    , refundCharge :: Text
    , refundBalanceTransaction :: Maybe Text
    } deriving (Show, Eq)

instance FromJSON Refund where
   parseJSON (Object o) = 
        Refund <$> o .: "id"
               <*> o .: "amount"
               <*> o .: "currency"
               <*> (fromSeconds <$> o .: "created")
               <*> o .: "charge"
               <*> o .:? "balance_transaction"
