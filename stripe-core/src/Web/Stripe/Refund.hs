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
-- {-\# LANGUAGE OverloadedStrings \#-}
-- import Web.Stripe
-- import Web.Stripe.Customer
-- import Web.Stripe.Charge
-- import Web.Stripe.Refund
--
-- main :: IO ()
-- main = do
--   let config = StripeConfig (StripeKey "secret_key")
--       credit = CardNumber "4242424242424242"
--       em  = ExpMonth 12
--       ey  = ExpYear 2015
--       cvc = CVC "123"
--       cardinfo = (mkNewCard credit em ey) { newCardCVC = Just cvc }
--   result <- stripe config $ createCustomer -&- cardinfo
--   case result of
--     (Left stripeError) -> print stripeError
--     (Right (Customer { customerId = cid })) -> do
--       result <- stripe config $ createCharge (Amount 100) USD -&- cid
--       case result of
--         (Left stripeError) -> print stripeError
--         (Right (Charge { chargeId   = chid })) -> do
--           result <- stripe config $ createRefund chid
--           case result of
--             (Left stripeError) -> print stripeError
--             (Right refund)     -> print refund
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
-- | create a `Refund`
createRefund
    :: ChargeId -- ^ `ChargeId` associated with the `Charge` to be refunded
    -> StripeRequest CreateRefund
createRefund
    chargeid    = request
  where request = mkStripeRequest POST url params
        url     = "charges" </> getChargeId chargeid </> "refunds"
        params  = []

data CreateRefund
type instance StripeReturn CreateRefund = Refund
instance StripeHasParam CreateRefund Amount
instance StripeHasParam CreateRefund RefundApplicationFee
instance StripeHasParam CreateRefund RefundReason
instance StripeHasParam CreateRefund MetaData

------------------------------------------------------------------------------
-- | Retrieve a `Refund` by `ChargeId` and `RefundId`
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

data GetRefund
type instance StripeReturn GetRefund = Refund
instance StripeHasParam GetRefund ExpandParams

------------------------------------------------------------------------------
-- | Update a `Refund` by `ChargeId` and `RefundId`
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

data UpdateRefund
type instance StripeReturn UpdateRefund = Refund
instance StripeHasParam UpdateRefund MetaData

------------------------------------------------------------------------------
-- | Retrieve a lot of Refunds by `ChargeId`
getRefunds
    :: ChargeId               -- ^ `ChargeId` associated with the `Refunds` to get
    -> StripeRequest GetRefunds
getRefunds
  chargeid = request
  where request = mkStripeRequest GET url params
        url     = "charges" </> getChargeId chargeid </> "refunds"
        params  = []

data GetRefunds
type instance StripeReturn GetRefunds = StripeList Refund
instance StripeHasParam GetRefunds ExpandParams
instance StripeHasParam GetRefunds (EndingBefore RefundId)
instance StripeHasParam GetRefunds Limit
instance StripeHasParam GetRefunds (StartingAfter RefundId)
