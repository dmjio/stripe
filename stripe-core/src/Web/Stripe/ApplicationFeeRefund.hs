{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
-------------------------------------------
-- |
-- Module      : Web.Stripe.AppplicationFeeRefund
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- < https:/\/\stripe.com/docs/api#fee_refunds >
--
-- @
-- import Web.Stripe
-- import Web.Stripe.ApplicationFee
--
-- main :: IO ()
-- main = do
--   let config = SecretKey "secret_key"
--   result <- stripe config $ getApplicationFeeRefund (FeeId "fee_id") (RefundId "refund_id")
--   case result of
--     Right applicationFeeRefund -> print applicationFeeRefund
--     Left stripeError           -> print stripeError
-- @
module Web.Stripe.ApplicationFeeRefund
    ( -- * API
      CreateApplicationFeeRefund
    , createApplicationFeeRefund
    , GetApplicationFeeRefund
    , getApplicationFeeRefund
    , getApplicationFeeRefundExpandable
    , UpdateApplicationFeeRefund
    , updateApplicationFeeRefund
    , GetApplicationFeeRefunds
    , getApplicationFeeRefunds
    , getApplicationFeeRefundsExpandable
      -- * Types
    , FeeId                  (..)
    , RefundId               (..)
    , ApplicationFee         (..)
    , ApplicationFeeRefund   (..)
    , StripeList             (..)
    , EndingBefore           (..)
    , StartingAfter          (..)
    , Limit                  (..)
    , ExpandParams
    , MetaData               (..)
    , Amount                 (..)
    ) where

import           Web.Stripe.StripeRequest (Method (GET, POST, DELETE), Param(..),
                                           StripeHasParam, StripeRequest (..),
                                           StripeReturn, ToStripeParam(..),
                                           mkStripeRequest)
import           Web.Stripe.Util          (toExpandable, (</>))
import           Web.Stripe.Types         (Amount(..), ApplicationFee (..),
                                           ApplicationFeeRefund (..),
                                           EndingBefore(..), ExpandParams,
                                           FeeId (..), Limit(..), MetaData(..),
                                           RefundId (..), StartingAfter(..),
                                           StripeList (..))

------------------------------------------------------------------------------
-- | Create a new `ApplicationFeeRefund`
data CreateApplicationFeeRefund
type instance StripeReturn CreateApplicationFeeRefund = ApplicationFeeRefund
instance StripeHasParam CreateApplicationFeeRefund Amount
instance StripeHasParam CreateApplicationFeeRefund MetaData
createApplicationFeeRefund
    :: FeeId        -- ^ The `FeeID` associated with the `ApplicationFee`
    -> StripeRequest CreateApplicationFeeRefund
createApplicationFeeRefund
    (FeeId feeid)
                = request
  where request = mkStripeRequest POST url params
        url     = "application_fees" </> feeid </> "refunds"
        params  = []

------------------------------------------------------------------------------
-- | Retrieve an existing 'ApplicationFeeRefund'
data GetApplicationFeeRefund
type instance StripeReturn GetApplicationFeeRefund = ApplicationFeeRefund
getApplicationFeeRefund
    :: FeeId     -- ^ The `FeeID` associated with the `ApplicationFee`
    -> RefundId  -- ^ The `ReufndId` associated with the `ApplicationFeeRefund`
    -> StripeRequest GetApplicationFeeRefund
getApplicationFeeRefund feeid refundid =
  getApplicationFeeRefundExpandable feeid refundid []

------------------------------------------------------------------------------
-- | Retrieve an existing 'ApplicationFeeRefund'
getApplicationFeeRefundExpandable
    :: FeeId          -- ^ The `FeeID` associated with the `ApplicationFee`
    -> RefundId       -- ^ The `ReufndId` associated with the `ApplicationFeeRefund`
    -> ExpandParams   -- ^ The `ExpandParams` to be used for object expansion
    -> StripeRequest GetApplicationFeeRefund
getApplicationFeeRefundExpandable (FeeId feeid) (RefundId refundid) expansion
    = request
  where request = mkStripeRequest GET url params
        url     = "application_fees" </> feeid </> "refunds" </> refundid
        params  = toExpandable expansion

------------------------------------------------------------------------------
-- | Update an `ApplicationFeeRefund` for a given Application `FeeId` and `RefundId`
data UpdateApplicationFeeRefund
type instance StripeReturn UpdateApplicationFeeRefund = ApplicationFeeRefund
instance StripeHasParam UpdateApplicationFeeRefund MetaData
updateApplicationFeeRefund
    :: FeeId    -- ^ The `FeeID` associated with the application
    -> RefundId -- ^ The `RefundId` associated with the application
    -> StripeRequest UpdateApplicationFeeRefund
updateApplicationFeeRefund
    (FeeId feeid)
    (RefundId refundid)
            = request
  where
    request = mkStripeRequest GET url params
    url     = "application_fees" </> feeid </> "refunds" </> refundid
    params  = []

------------------------------------------------------------------------------
-- | Retrieve a list of all 'ApplicationFeeRefund's for a given Application 'FeeId'
data GetApplicationFeeRefunds
type instance StripeReturn GetApplicationFeeRefunds = (StripeList ApplicationFeeRefund)
instance StripeHasParam GetApplicationFeeRefunds (EndingBefore RefundId)
instance StripeHasParam GetApplicationFeeRefunds Limit
instance StripeHasParam GetApplicationFeeRefunds (StartingAfter RefundId)
getApplicationFeeRefunds
    :: FeeId               -- ^ The `FeeID` associated with the application
    -> StripeRequest GetApplicationFeeRefunds
getApplicationFeeRefunds
   feeid =
     getApplicationFeeRefundsExpandable
       feeid []

------------------------------------------------------------------------------
-- | Retrieve a list of all 'ApplicationFeeRefund's for a given Application 'FeeId'
getApplicationFeeRefundsExpandable
    :: FeeId               -- ^ The `FeeID` associated with the `ApplicationFee`
    -> ExpandParams        -- ^ The `ExpandParams` to be used for object expansion
    -> StripeRequest GetApplicationFeeRefunds
getApplicationFeeRefundsExpandable
  (FeeId feeid)
   expandParams = request
  where
    request = mkStripeRequest GET url params
    url     = "application_fees" </> feeid </> "refunds"
    params  = toExpandable expandParams

