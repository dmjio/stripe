{-# LANGUAGE OverloadedStrings #-}
module Web.Stripe.ApplicationFee
    (  -- * API
      getApplicationFee
    , getApplicationFees
       -- * Types
    , ApplicationFee (..)
    , FeeId          (..)
    , StripeList     (..)
    ) where

import           Web.Stripe.Client.Internal (Method (GET), Stripe,
                                             StripeRequest (..), callAPI, (</>))
import           Web.Stripe.Types           (ApplicationFee, FeeId (..),
                                             StripeList)

------------------------------------------------------------------------------
-- | 'ApplicationFee' retrieval
getApplicationFee
    :: FeeId -- ^ The FeeID associated with the application
    -> Stripe ApplicationFee
getApplicationFee
    (FeeId feeId) = callAPI request
  where request = StripeRequest GET url params
        url     = "application_fees" </> feeId
        params  = []

------------------------------------------------------------------------------
-- | 'ApplicationFee's retrieval
getApplicationFees :: Stripe (StripeList ApplicationFee)
getApplicationFees = callAPI request
  where request = StripeRequest GET url params
        url     = "application_fees"
        params  = []


