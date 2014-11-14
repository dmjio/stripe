{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
-------------------------------------------
-- |
-- Module      : Web.Stripe.Refund
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- < https:/\/\stripe.com/docs/api#refunds >
--
-- @
-- import Web.Stripe
-- import Web.Stripe.Customer
-- import Web.Stripe.Charge
-- import Web.Stripe.Refund
--
-- main :: IO ()
-- main = do
--   let config = SecretKey "secret_key"
--       credit = CardNumber "4242424242424242"
--       em  = ExpMonth 12
--       ey  = ExpYear 2015
--       cvc = CVC "123"
--   result <- stripe config $ do
--     Customer { customerId = cid }  <- createCustomerByCard cn em ey cvc
--     Charge   { chargeId   = chid } <- chargeCustomer cid USD 100 Nothing
--     createRefund chid ([] :: MetaData)
--   case result of
--     Right refund     -> print refund
--     Left stripeError -> print stripeError
-- @
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

import           Web.Stripe.Client.Types    (Method (GET, POST),
                                             StripeRequest (..), mkStripeRequest)
import           Web.Stripe.Client.Util     (getParams, toMetaData, toText,
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
    :: ChargeId -- ^ `ChargeId` associated with the `Charge` to be refunded
    -> MetaData -- ^ `MetaData` associated with a `Refund`
    -> StripeRequest Refund
createRefund
    chargeid
    metadata    = request
  where request = mkStripeRequest POST url params
        url     = "charges" </> getChargeId chargeid </> "refunds"
        params  = toMetaData metadata

------------------------------------------------------------------------------
-- | Retrieve a `Refund` by `ChargeId` and `RefundId`
getRefund
    :: ChargeId -- ^ `ChargeId` associated with the `Refund` to be retrieved
    -> RefundId -- ^ `RefundId` associated with the `Refund` to be retrieved
    -> StripeRequest Refund
getRefund
    chargeid
    (RefundId refundid) = request
   where request = mkStripeRequest GET url params
         url     = "charges" </> getChargeId chargeid </> "refunds" </> refundid
         params  = []

------------------------------------------------------------------------------
-- | Retrieve a `Refund` by `ChargeId` and `RefundId` with `ExpandParams`
getRefundExpandable
    :: ChargeId     -- ^ `ChargeId` associated with the `Charge` to be retrieved
    -> RefundId     -- ^ `RefundId` associated with the `Refund` to be retrieved
    -> ExpandParams -- ^ `ExpandParams` of object for expansion
    -> StripeRequest Refund
getRefundExpandable
    chargeid
    (RefundId refundid)
    expandParams = request
   where request = mkStripeRequest GET url params
         url     = "charges" </> getChargeId chargeid </> "refunds" </> refundid
         params  = toExpandable expandParams

------------------------------------------------------------------------------
-- | Update a `Refund` by `ChargeId` and `RefundId`
updateRefund
    :: ChargeId -- ^ `ChargeId` associated with the `Charge` to be updated
    -> RefundId -- ^ `RefundId` associated with the `Refund` to be retrieved
    -> MetaData -- ^ `MetaData` associated with a `Refund`
    -> StripeRequest Refund
updateRefund
   chargeid
   (RefundId refid)
   metadata     = request
  where request = mkStripeRequest POST url params
        url     = "charges" </> getChargeId chargeid  </> "refunds" </> refid
        params  = toMetaData metadata

------------------------------------------------------------------------------
-- | Retrieve a lot of Refunds by `ChargeId`
getRefunds
    :: ChargeId               -- ^ `ChargeId` associated with the `Charge` to be updated
    -> Limit                  -- ^ Defaults to 10 if `Nothing` specified
    -> StartingAfter RefundId -- ^ Paginate starting after the following `RefundId`
    -> EndingBefore RefundId  -- ^ Paginate ending before the following `RefundId`
    -> StripeRequest (StripeList Refund)
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
    :: ChargeId               -- ^ `ChargeId` associated with the `Charge` to be updated
    -> Limit                  -- ^ Defaults to 10 if `Nothing` specified
    -> StartingAfter RefundId -- ^ Paginate starting after the following `RefundId`
    -> EndingBefore RefundId  -- ^ Paginate ending before the following `RefundId`
    -> ExpandParams           -- ^ `MetaData` associated with a `Refund`
    -> StripeRequest (StripeList Refund)
getRefundsExpandable
  chargeid
  limit
  startingAfter
  endingBefore
  expandParams  = request
  where request = mkStripeRequest GET url params
        url     = "charges" </> getChargeId chargeid </> "refunds"
        params  = getParams [
            ("limit", toText `fmap` limit )
          , ("starting_after", (\(RefundId x) -> x) `fmap` startingAfter)
          , ("ending_before", (\(RefundId x) -> x) `fmap` endingBefore)
          ] ++ toExpandable expandParams
