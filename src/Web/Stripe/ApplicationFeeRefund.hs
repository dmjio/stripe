{-# LANGUAGE OverloadedStrings #-}
module Web.Stripe.ApplicationFeeRefund
    ( -- * API
      createApplicationFeeRefund
    , getApplicationFeeRefund
    , getApplicationFeeRefunds
    , updateApplicationFeeRefund
      -- * Types
    , FeeId                  (..)
    , RefundId               (..)
    , ApplicationFee         (..)
    , ApplicationFeeRefund   (..)
    , StripeList             (..)
    , Amount
    ) where

import           Web.Stripe.Client.Internal (Method (POST, GET), Stripe,
                                             StripeRequest (..), callAPI,toMetaData, 
                                             getParams, toText, (</>))
import           Web.Stripe.Types           (Amount, ApplicationFee (..),
                                             ApplicationFeeRefund (..),  MetaData,
                                             EndingBefore, FeeId (..), Limit,
                                             RefundId (..), StartingAfter,
                                             StripeList (..))

------------------------------------------------------------------------------
-- | Create a new 'ApplicationFeeRefund'
createApplicationFeeRefund
    :: FeeId        -- ^ The FeeID associated with the application
    -> Maybe Amount -- ^ The Amount associated with the Fee (optional)
    -> MetaData     -- ^ The MetaData associated with the Fee (optional)
    -> Stripe ApplicationFeeRefund
createApplicationFeeRefund
    (FeeId feeid)
    amount
    metadata    = callAPI request
  where request = StripeRequest POST url params
        url     = "application_fees" </> feeid </> "refunds"
        params  = toMetaData metadata ++ getParams [
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

------------------------------------------------------------------------------
-- | Update an `ApplicationFeeRefund` for a given Application `FeeId` and `RefundId`
updateApplicationFeeRefund
    :: RefundId -- ^ The `RefundId` associated with the application
    -> FeeId    -- ^ The `FeeID` associated with the application
    -> MetaData -- ^ The `MetaData` associated with the Fee (optional)
    -> Stripe (StripeList ApplicationFeeRefund)
updateApplicationFeeRefund
    (RefundId refundid)
    (FeeId feeid)
    metadata = callAPI request
  where
    request = StripeRequest GET url params
    url     = "application_fees" </> feeid </> "refunds"
    params  = toMetaData metadata

