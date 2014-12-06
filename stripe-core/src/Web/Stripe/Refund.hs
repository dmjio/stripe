{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
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
      CreateRefund
    , createRefund
    , GetRefund
    , getRefund
    , GetRefunds
    , getRefunds
    , UpdateRefund
    , updateRefund
      -- * Types
    , Amount       (..)
    , Charge       (..)
    , ChargeId     (..)
    , EndingBefore (..)
    , ExpandParams (..)
    , Refund       (..)
    , RefundApplicationFee(..)
    , RefundReason (..)
    , RefundId     (..)
    , StripeList   (..)
    ) where

import           Web.Stripe.StripeRequest   (Method (GET, POST),
                                             StripeHasParam, StripeReturn,
                                             StripeRequest (..), mkStripeRequest)
import           Web.Stripe.Util            ((</>))
import           Web.Stripe.Types           (Amount(..), Charge (..), ChargeId (..),
                                             EndingBefore(..), Limit(..),
                                             MetaData(..), Refund (..),
                                             RefundApplicationFee(..),
                                             RefundId (..), RefundReason(..),
                                             StartingAfter(..), ExpandParams(..),
                                             StripeList (..))
import           Web.Stripe.Types.Util      (getChargeId)

------------------------------------------------------------------------------
-- | `Refund` a `Charge`
data CreateRefund
type instance StripeReturn CreateRefund = Refund
instance StripeHasParam CreateRefund Amount
instance StripeHasParam CreateRefund RefundApplicationFee
instance StripeHasParam CreateRefund RefundReason
instance StripeHasParam CreateRefund MetaData
createRefund
    :: ChargeId -- ^ `ChargeId` associated with the `Charge` to be refunded
    -> StripeRequest CreateRefund
createRefund
    chargeid    = request
  where request = mkStripeRequest POST url params
        url     = "charges" </> getChargeId chargeid </> "refunds"
        params  = []

------------------------------------------------------------------------------
-- | Retrieve a `Refund` by `ChargeId` and `RefundId`
data GetRefund
type instance StripeReturn GetRefund = Refund
instance StripeHasParam GetRefund ExpandParams
getRefund
    :: ChargeId -- ^ `ChargeId` associated with the `Refund` to be retrieved
    -> RefundId -- ^ `RefundId` associated with the `Refund` to be retrieved
    -> StripeRequest GetRefund
getRefund
   chargeid
   (RefundId refundid) = request
   where request = mkStripeRequest GET url params
         url     = "charges" </> getChargeId chargeid </> "refunds" </> refundid
         params  = []

------------------------------------------------------------------------------
-- | Update a `Refund` by `ChargeId` and `RefundId`
data UpdateRefund
type instance StripeReturn UpdateRefund = Refund
instance StripeHasParam UpdateRefund MetaData
updateRefund
    :: ChargeId -- ^ `ChargeId` associated with the `Charge` to be updated
    -> RefundId -- ^ `RefundId` associated with the `Refund` to be retrieved
    -> StripeRequest UpdateRefund
updateRefund
   chargeid
   (RefundId refid)
                = request
  where request = mkStripeRequest POST url params
        url     = "charges" </> getChargeId chargeid  </> "refunds" </> refid
        params  = []

------------------------------------------------------------------------------
-- | Retrieve a lot of Refunds by `ChargeId`
data GetRefunds
type instance StripeReturn GetRefunds = StripeList Refund
instance StripeHasParam GetRefunds ExpandParams
instance StripeHasParam GetRefunds (EndingBefore RefundId)
instance StripeHasParam GetRefunds Limit
instance StripeHasParam GetRefunds (StartingAfter RefundId)
getRefunds
    :: ChargeId               -- ^ `ChargeId` associated with the `Charge` to be updated
    -> StripeRequest GetRefunds
getRefunds
  chargeid = request
  where request = mkStripeRequest GET url params
        url     = "charges" </> getChargeId chargeid </> "refunds"
        params  = []
