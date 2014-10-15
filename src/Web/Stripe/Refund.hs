{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
-- |
-- Module      : Web.Stripe.Refund
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Web.Stripe.Refund
    ( -- * API
      createRefund
    , getRefund
    , getRefundExpandable
    , getRefunds
    , getRefundsExpandable
    , updateRefund
      -- * Types
    , Refund     (..)
    , RefundId   (..)
    , ChargeId   (..)
    , Charge     (..)
    , StripeList (..)
    ) where

import           Web.Stripe.Client.Internal (Method (GET, POST), Stripe,
                                             StripeRequest (..), callAPI,
                                             getParams, toMetaData, toText,
                                             (</>), toExpandable)
import           Web.Stripe.Types           (Charge (..), ChargeId (..),
                                             EndingBefore, Limit, MetaData,
                                             Refund (..), Refund (..),
                                             RefundId (..), StartingAfter, ExpandParams,
                                             StripeList (..))
import           Web.Stripe.Types.Util      (getChargeId)

------------------------------------------------------------------------------
-- | `Refund` a `Charge`
createRefund
    :: ChargeId -- ^ 'ChargeId' associated with the 'Charge' to be refunded
    -> MetaData -- ^ `MetaData` associated with a `Refund`
    -> Stripe Refund
createRefund
    chargeid
    metadata    = callAPI request
  where request = StripeRequest POST url params
        url     = "charges" </> getChargeId chargeid </> "refunds"
        params  = toMetaData metadata

------------------------------------------------------------------------------
-- | Retrieve a `Refund` by `ChargeId` and `RefundId`
getRefund
    :: ChargeId -- ^ 'ChargeId' associated with the 'Charge' to be retrieved
    -> RefundId -- ^ 'RefundId' associated with the 'Refund' to be retrieved
    -> Stripe Refund
getRefund
    chargeid
    (RefundId refundid) = callAPI request
   where request = StripeRequest GET url params
         url     = "charges" </> getChargeId chargeid </> "refunds" </> refundid
         params  = []

------------------------------------------------------------------------------
-- | Retrieve a `Refund` by `ChargeId` and `RefundId` with `ExpandParams`
getRefundExpandable
    :: ChargeId -- ^ 'ChargeId' associated with the 'Charge' to be retrieved
    -> RefundId -- ^ 'RefundId' associated with the 'Refund' to be retrieved
    -> ExpandParams
    -> Stripe Refund
getRefundExpandable
    chargeid
    (RefundId refundid)
    expandParams = callAPI request
   where request = StripeRequest GET url params
         url     = "charges" </> getChargeId chargeid </> "refunds" </> refundid
         params  = toExpandable expandParams

------------------------------------------------------------------------------
-- | Update a `Refund` by `ChargeId` and `RefundId`
updateRefund
    :: ChargeId -- ^ 'ChargeId' associated with the 'Charge' to be updated
    -> RefundId -- ^ 'RefundId' associated with the 'Refund' to be retrieved
    -> MetaData -- ^ `MetaData` associated with a `Refund`
    -> Stripe Refund
updateRefund
   chargeid
   (RefundId refid)
   metadata     = callAPI request
  where request = StripeRequest POST url params
        url     = "charges" </> getChargeId chargeid  </> "refunds" </> refid
        params  = toMetaData metadata

------------------------------------------------------------------------------
-- | Retrieve a lot of Refunds by `ChargeId`
getRefunds
    :: ChargeId               -- ^ 'ChargeId' associated with the 'Charge' to be updated
    -> Limit                  -- ^ Defaults to 10 if `Nothing` specified
    -> StartingAfter RefundId -- ^ Paginate starting after the following `RefundId`
    -> EndingBefore RefundId  -- ^ Paginate ending before the following `RefundId`
    -> Stripe (StripeList Refund)
getRefunds
  chargeid
  limit
  startingAfter
  endingBefore  =
    getRefundsExpandable chargeid
      limit startingAfter endingBefore []

------------------------------------------------------------------------------
-- | Retrieve a lot of Refunds by `ChargeId` with `ExpandParams`
getRefundsExpandable
    :: ChargeId               -- ^ 'ChargeId' associated with the 'Charge' to be updated
    -> Limit                  -- ^ Defaults to 10 if `Nothing` specified
    -> StartingAfter RefundId -- ^ Paginate starting after the following `RefundId`
    -> EndingBefore RefundId  -- ^ Paginate ending before the following `RefundId`
    -> ExpandParams
    -> Stripe (StripeList Refund)
getRefundsExpandable
  chargeid
  limit
  startingAfter
  endingBefore
  expandParams  = callAPI request
  where request = StripeRequest GET url params
        url     = "charges" </> getChargeId chargeid </> "refunds"
        params  = getParams [
            ("limit", toText `fmap` limit )
          , ("starting_after", (\(RefundId x) -> x) `fmap` startingAfter)
          , ("ending_before", (\(RefundId x) -> x) `fmap` endingBefore)
          ] ++ toExpandable expandParams



