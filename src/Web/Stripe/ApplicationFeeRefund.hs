{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Web.Stripe.ApplicationFeeRefund
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Web.Stripe.ApplicationFeeRefund
    ( -- * API
      createApplicationFeeRefund
    , getApplicationFeeRefund
    , getApplicationFeeRefundExpandable
    , getApplicationFeeRefunds
    , getApplicationFeeRefundsExpandable
    , updateApplicationFeeRefund
      -- * Types
    , FeeId                  (..)
    , RefundId               (..)
    , ApplicationFee         (..)
    , ApplicationFeeRefund   (..)
    , StripeList             (..)
    , EndingBefore
    , StartingAfter
    , Limit
    , ExpandParams
    , MetaData
    , Amount
    ) where

import           Web.Stripe.Client.Internal (Method (POST, GET), Stripe,
                                             StripeRequest (..), callAPI,
                                             getParams, toExpandable,
                                             toMetaData, toText, (</>))
import           Web.Stripe.Types           (Amount, ApplicationFee (..),
                                             ApplicationFeeRefund (..),
                                             EndingBefore, ExpandParams,
                                             FeeId (..), Limit, MetaData,
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
getApplicationFeeRefund feeid refundid =
  getApplicationFeeRefundExpandable feeid refundid []

------------------------------------------------------------------------------
-- | Retrieve an existing 'ApplicationFeeRefund'
getApplicationFeeRefundExpandable
    :: FeeId
    -> RefundId
    -> ExpandParams
    -> Stripe ApplicationFeeRefund
getApplicationFeeRefundExpandable (FeeId feeid) (RefundId refundid) expansion
    = callAPI request
  where request = StripeRequest GET url params
        url     = "application_fees" </> feeid </> "refunds" </> refundid
        params  = toExpandable expansion

------------------------------------------------------------------------------
-- | Retrieve a list of all 'ApplicationFeeRefund's for a given Application 'FeeId'
getApplicationFeeRefunds
    :: FeeId               -- ^ The `FeeID` associated with the application
    -> Limit               -- ^ Limit on how many Refunds to return (max 100, default 10)
    -> StartingAfter FeeId -- ^ Lower bound on how many Refunds to return
    -> EndingBefore FeeId  -- ^ Upper bound on how many Refunds to return
    -> Stripe (StripeList ApplicationFeeRefund)
getApplicationFeeRefunds
   feeid
   limit
   startingAfter
   endingBefore =
     getApplicationFeeRefundsExpandable
       feeid limit startingAfter endingBefore []

------------------------------------------------------------------------------
-- | Retrieve a list of all 'ApplicationFeeRefund's for a given Application 'FeeId'
getApplicationFeeRefundsExpandable
    :: FeeId   -- ^ The `FeeID` associated with the application
    -> Limit   -- ^ Limit on how many Refunds to return (max 100, default 10)
    -> StartingAfter FeeId -- ^ Lower bound on how many Refunds to return
    -> EndingBefore FeeId  -- ^ Upper bound on how many Refunds to return
    -> ExpandParams
    -> Stripe (StripeList ApplicationFeeRefund)
getApplicationFeeRefundsExpandable
  (FeeId feeid)
   limit
   startingAfter
   endingBefore
   expandParams = callAPI request
  where
    request = StripeRequest GET url params
    url     = "application_fees" </> feeid </> "refunds"
    params  = getParams [
        ("limit", toText `fmap` limit )
      , ("starting_after", (\(FeeId x) -> x) `fmap` startingAfter)
      , ("ending_before", (\(FeeId x) -> x) `fmap` endingBefore)
      ] ++ toExpandable expandParams

------------------------------------------------------------------------------
-- | Update an `ApplicationFeeRefund` for a given Application `FeeId` and `RefundId`
updateApplicationFeeRefund
    :: FeeId    -- ^ The `FeeID` associated with the application
    -> RefundId -- ^ The `RefundId` associated with the application
    -> MetaData -- ^ The `MetaData` associated with the Fee (optional)
    -> Stripe (StripeList ApplicationFeeRefund)
updateApplicationFeeRefund
    (FeeId feeid)
    (RefundId refundid)
    metadata = callAPI request
  where
    request = StripeRequest GET url params
    url     = "application_fees" </> feeid </> "refunds" </> refundid
    params  = toMetaData metadata

