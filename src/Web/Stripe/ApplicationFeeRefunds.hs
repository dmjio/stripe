{-# LANGUAGE OverloadedStrings #-}
module Web.Stripe.ApplicationFeeRefunds
    ( -- * API
      createApplicationFeeRefund
    , getApplicationFeeRefund
    , getApplicationFeeRefunds
      -- * Types
    , FeeId                  (..)
    , RefundId               (..)
    , ApplicationFee         (..)
    , ApplicationFeeRefund   (..)
    , StripeList             (..)
    , Amount
    ) where

import           Web.Stripe.Client.Internal (Method (POST, GET), Stripe,
                                             StripeRequest (..), callAPI,
                                             getParams, toText, (</>))
import           Web.Stripe.Types           (Amount, ApplicationFee (..),
                                             ApplicationFeeRefund (..),
                                             EndingBefore, FeeId (..), Limit,
                                             RefundId (..), StartingAfter,
                                             StripeList (..))

------------------------------------------------------------------------------
-- | Create a new 'ApplicationFeeRefund'
createApplicationFeeRefund
    :: FeeId        -- ^ The FeeID associated with the application
    -> Maybe Amount -- ^ The Amount associated with the Fee (optional)
    -> Stripe ApplicationFeeRefund
createApplicationFeeRefund
    (FeeId feeid)
    amount      = callAPI request
  where request = StripeRequest POST url params
        url     = "application_fees" </> feeid </> "refunds"
        params  = getParams [
                   ("amount", fmap toText amount)
                  ]

------------------------------------------------------------------------------
-- | Retrieve an existing 'ApplicationFeeRefund'
getApplicationFeeRefund
    :: FeeId
    -> RefundId
    -> Stripe ApplicationFeeRefund
getApplicationFeeRefund (FeeId feeid) (RefundId refundid)
    = callAPI request
  where request = StripeRequest GET url params
        url     = "application_fees" </> feeid </> "refunds" </> refundid
        params  = []

------------------------------------------------------------------------------
-- | Retrieve a list of all 'ApplicationFeeRefund's for a given Application 'FeeId'
getApplicationFeeRefunds
    :: FeeId
    -> Limit
    -> StartingAfter FeeId
    -> EndingBefore FeeId
    -> Stripe (StripeList ApplicationFeeRefund)
getApplicationFeeRefunds (FeeId feeid)
   limit
   startingAfter
   endingBefore
    = callAPI request
  where
    request = StripeRequest GET url params
    url     = "application_fees" </> feeid </> "refunds"
    params  = getParams [
        ("limit", toText `fmap` limit )
      , ("starting_after", (\(FeeId x) -> x) `fmap` startingAfter)
      , ("ending_before", (\(FeeId x) -> x) `fmap` endingBefore)
      ]

