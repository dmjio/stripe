module Web.Stripe.ApplicationFee
    ( ApplicationFee(..)
    , FeeId(..)
    , getApplicationFee
    , refundApplicationFee
    ) where

import           Data.Monoid
import           Web.Stripe.Client.Internal
import           Web.Stripe.Types                (ApplicationFee(..))

getApplicationFee :: FeeId -> Stripe ApplicationFee
getApplicationFee (FeeId feeId) = callAPI request
  where request = StripeRequest GET url []
        url     = "application_fees/" <> feeId

refundApplicationFee :: FeeId -> Stripe ApplicationFee
refundApplicationFee (FeeId feeId) = callAPI request
  where request = StripeRequest POST url []
        url     = "application_fees/" <> feeId <> "/refund"

