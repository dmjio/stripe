{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE OverloadedStrings     #-}
-- |
-- Module      : Web.Stripe.StripeRequest
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Web.Stripe.StripeRequest
  ( -- * Types
    Method(..)
  , Param(..)
  , Params
  , StripeRequest (..)
  , StripeReturn
  , StripeHasParam
  , ToStripeParam(..)
  , (-&-)
  , mkStripeRequest
  ) where

import           Data.Aeson         (FromJSON, eitherDecodeStrict)
import           Data.ByteString    (ByteString)
import           Data.String        (fromString)
import           Data.Text          (Text)
import qualified Data.Text.Encoding as Text
import           Numeric            (showFFloat)
import           Web.Stripe.Error   (StripeError(..))
import           Web.Stripe.Types   (AccountBalance(..), AddressCity(..), AddressCountry(..), AddressLine1(..), AddressLine2(..), AddressState(..), AddressZip(..), Amount(..), ApplicationFeeAmount(..), ApplicationFeePercent(..), AtPeriodEnd(..), Card(..), CardId(..), CardNumber(..), Capture(..), ChargeId(..), CouponId(..), Created(..), Currency(..), CustomerId(..), CVC(..), Description(..), Email(..), EndingBefore(..), ExpMonth(..), ExpYear(..), Interval(..), IntervalCount(..), MetaData(..), PlanId(..), PlanName(..), Prorate(..), Limit(..), Name(..), Quantity(..), ReceiptEmail(..), RefundId(..), RefundApplicationFee(..), RefundReason(..), StartingAfter(..), StatementDescription(..), SubscriptionId(..), TokenId(..), TrialEnd(..), TrialPeriodDays(..))
import           Web.Stripe.Util    (toBytestring, toMetaData, toSeconds)

------------------------------------------------------------------------------
-- | HTTP Method
--
-- The other methods are not required by the Stripe API
data Method
  = DELETE
  | GET
  | POST
    deriving (Eq, Ord, Read, Show)

------------------------------------------------------------------------------
-- | HTTP Params
type Params = [(ByteString, ByteString)]

------------------------------------------------------------------------------
-- | used to set a specific key/value pair when the type is not enough
newtype Param k v = Param (k, v)

------------------------------------------------------------------------------
-- | Stripe Request holding `Method`, URL and `Params` for a Request. Also
-- includes the function needed to decode the response.
--
data StripeRequest a = StripeRequest
    { method      :: Method -- ^ Method of StripeRequest (i.e. `GET`, `PUT`, `POST`, `PUT`)
    , endpoint    :: Text   -- ^ Endpoint of StripeRequest
    , queryParams :: Params -- ^ Query Parameters of StripeRequest
    , decodeJson  :: ByteString -> Either String (StripeReturn a)
    } -- deriving Functor

------------------------------------------------------------------------------
-- | convert a parameter to a key/value
class ToStripeParam param where
  toStripeParam :: param -> [(ByteString, ByteString)] -> [(ByteString, ByteString)]

instance ToStripeParam Amount where
  toStripeParam (Amount i) =
    (("amount", toBytestring i) :)

instance ToStripeParam AccountBalance where
  toStripeParam (AccountBalance i) =
    (("account_balance", toBytestring i) :)

instance ToStripeParam AddressCity where
  toStripeParam (AddressCity txt) =
    (("address_city", Text.encodeUtf8 txt) :)

instance ToStripeParam AddressCountry where
  toStripeParam (AddressCountry txt) =
    (("address_country", Text.encodeUtf8 txt) :)

instance ToStripeParam AddressLine1 where
  toStripeParam (AddressLine1 txt) =
    (("address_line1", Text.encodeUtf8 txt) :)

instance ToStripeParam AddressLine2 where
  toStripeParam (AddressLine2 txt) =
    (("address_line2", Text.encodeUtf8 txt) :)

instance ToStripeParam AddressState where
  toStripeParam (AddressState txt) =
    (("address_state", Text.encodeUtf8 txt) :)

instance ToStripeParam AddressZip where
  toStripeParam (AddressZip txt) =
    (("address_zip", Text.encodeUtf8 txt) :)

instance ToStripeParam ApplicationFeeAmount where
  toStripeParam (ApplicationFeeAmount cents) =
    (("application_fee", toBytestring cents) :)

instance ToStripeParam ApplicationFeePercent where
  toStripeParam (ApplicationFeePercent fee) =
    (("application_fee_percent", fromString $ showFFloat (Just 2) fee "") :)

instance ToStripeParam AtPeriodEnd where
  toStripeParam (AtPeriodEnd p) =
    (("prorate", if p then "true" else "false") :)

instance ToStripeParam Capture where
  toStripeParam (Capture b) =
    (("capture", if b then "true" else "false") :)

instance ToStripeParam Card where
  toStripeParam _ = error "FIXME: Added ToStripeParam for Card"

instance ToStripeParam CardId where
  toStripeParam (CardId cid) =
    (("card", Text.encodeUtf8 cid) :)

instance ToStripeParam CardNumber where
  toStripeParam (CardNumber num) =
    (("number", Text.encodeUtf8 num) :)

instance ToStripeParam ChargeId where
  toStripeParam (ChargeId cid) =
    (("charge", Text.encodeUtf8 cid) :)

instance ToStripeParam Created where
  toStripeParam (Created time) =
    (("created", toBytestring $ toSeconds time) :)

instance ToStripeParam Currency where
  toStripeParam currency =
    (("currency", toBytestring currency) :)

instance ToStripeParam CustomerId where
  toStripeParam (CustomerId cid) =
    (("customer", Text.encodeUtf8 cid) :)

instance ToStripeParam CouponId where
  toStripeParam (CouponId cid) =
    (("coupon", Text.encodeUtf8 cid) :)

instance ToStripeParam CVC where
  toStripeParam (CVC cvc) =
    (("cvc", Text.encodeUtf8 cvc) :)

instance ToStripeParam Description where
  toStripeParam (Description txt) =
    (("description", Text.encodeUtf8 txt) :)

instance ToStripeParam Email where
  toStripeParam (Email txt) =
    (("email", Text.encodeUtf8 txt) :)

instance ToStripeParam ExpMonth where
  toStripeParam (ExpMonth m) =
    (("exp_month", toBytestring m) :)

instance ToStripeParam ExpYear where
  toStripeParam (ExpYear y) =
    (("exp_year", toBytestring y) :)

instance ToStripeParam Interval where
  toStripeParam interval =
    (("interval", toBytestring interval) :)

instance ToStripeParam IntervalCount where
  toStripeParam (IntervalCount c) =
    (("interval_count", toBytestring c) :)

instance ToStripeParam Limit where
  toStripeParam (Limit i) =
    (("limit", toBytestring i) :)

instance ToStripeParam Name where
  toStripeParam (Name txt) =
    (("name", Text.encodeUtf8 txt) :)

instance ToStripeParam (Param Text Text) where
  toStripeParam (Param (k,v)) =
    ((Text.encodeUtf8 k, Text.encodeUtf8 v) :)

instance ToStripeParam PlanId where
  toStripeParam (PlanId pid) =
    (("plan", Text.encodeUtf8 pid) :)

instance ToStripeParam PlanName where
  toStripeParam (PlanName txt) =
    (("name", Text.encodeUtf8 txt) :)

instance ToStripeParam Prorate where
  toStripeParam (Prorate p) =
    (("prorate", if p then "true" else "false") :)

instance ToStripeParam Quantity where
  toStripeParam (Quantity i) =
    (("quantity", toBytestring i) :)

instance ToStripeParam RefundId where
  toStripeParam (RefundId fid) =
    (("refund", Text.encodeUtf8 fid) :)

instance ToStripeParam ReceiptEmail where
  toStripeParam (ReceiptEmail txt) =
    (("receipt_email", Text.encodeUtf8 txt) :)

instance ToStripeParam SubscriptionId where
  toStripeParam (SubscriptionId sid) =
    (("subscription", Text.encodeUtf8 sid) :)

instance ToStripeParam TokenId where
  toStripeParam (TokenId tid) =
    (("card", Text.encodeUtf8 tid) :)

instance ToStripeParam TrialEnd where
  toStripeParam (TrialEnd time) =
    (("trial_end", toBytestring $ toSeconds time) :)

instance ToStripeParam TrialPeriodDays where
  toStripeParam (TrialPeriodDays days) =
    (("trial_period_days", toBytestring days) :)

instance ToStripeParam MetaData where
  toStripeParam (MetaData kvs) =
    (toMetaData kvs ++)

instance ToStripeParam RefundApplicationFee where
  toStripeParam (RefundApplicationFee b) =
    (("refund_application_fee", if b then "true" else "false") :)

instance ToStripeParam RefundReason where
  toStripeParam reason =
    (("reason", case reason of
         RefundDuplicate -> "duplicate"
         RefundFraudulent -> "fraudulent"
         RefundRequestedByCustomer -> "requested_by_customer") :)

instance ToStripeParam StatementDescription where
  toStripeParam (StatementDescription txt) =
    (("statement_description", Text.encodeUtf8 txt) :)

instance (ToStripeParam param) => ToStripeParam (StartingAfter param) where
  toStripeParam (StartingAfter param) =
    case toStripeParam param [] of
      [(_, p)] -> (("starting_after", p) :)
      _        -> error "StartingAfter applied to non-singleton"

instance (ToStripeParam param) => ToStripeParam (EndingBefore param) where
  toStripeParam (EndingBefore param) =
    case toStripeParam param [] of
      [(_, p)] -> (("ending_before", p) :)
      _        -> error "EndingBefore applied to non-singleton"

------------------------------------------------------------------------------
-- | indicate if a request allows an optional parameter
class (ToStripeParam param) => StripeHasParam request param where

------------------------------------------------------------------------------
-- | add an optional parameter to a `StripeRequest`
(-&-) :: StripeHasParam request param =>
         StripeRequest request
      -> param
      -> StripeRequest request
stripeRequest -&- param =
  stripeRequest { queryParams =
                     toStripeParam param (queryParams stripeRequest)
                }

------------------------------------------------------------------------------
-- | return type of stripe request
type family StripeReturn a :: *

------------------------------------------------------------------------------
-- | HTTP Params
--
-- helper function for building a 'StripeRequest'
mkStripeRequest :: forall a. (FromJSON (StripeReturn a)) =>
                   Method
                -> Text
                -> Params
                -> StripeRequest a
mkStripeRequest m e q = StripeRequest m e q eitherDecodeStrict
