module Web.Stripe.Refunds 
    ( -- * Refund Types
      Refund (..)
    , RefundId (..)
      -- * API Functions
    , createRefund
    , getRefund
    , updateRefund
    ) where

import           Web.Stripe.Client.Internal
import           Web.Stripe.Util
import           Web.Stripe.Types
import qualified Data.Text.Encoding as T
import           Data.Monoid

createRefund :: ChargeId -> Stripe Refund
createRefund (ChargeId chargeId) = callAPI request 
  where request = StripeRequest POST url params
        url     = "charges" </> chargeId </> "refunds"
        params  = []

getRefund :: ChargeId -> RefundId -> Stripe Refund
getRefund (ChargeId chargeId) (RefundId refId) = callAPI request 
   where request = StripeRequest GET url params
         url     = "charges" </> chargeId </> "refunds" </> refId
         params  = []

updateRefund :: ChargeId -> RefundId -> Key -> Value -> Stripe Refund
updateRefund (ChargeId chargeId) (RefundId refId) key value = callAPI request 
  where request = StripeRequest POST url params
        url     = "charges" </> chargeId </> "refunds" </> refId
        params  = [ ("metadata[" <> T.encodeUtf8 key <> "]", T.encodeUtf8 value)]

-- getRefunds :: ChargeId -> Stripe Refunds
-- getRefunds (ChargeId chargeId) = callAPI request 
--   where request = StripeRequestuest GET url params
--         url     = "charges/" <> chargeId <> "/refunds"
--         params  = []

