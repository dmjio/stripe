{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
-------------------------------------------
-- |
-- Module      : Web.Stripe.AppplicationFeeRefund
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : code@dmj.io
-- Stability   : experimental
-- Portability : POSIX
--
-- < https:/\/\stripe.com/docs/api#fee_refunds >
--
-- @
-- {-\# LANGUAGE OverloadedStrings \#-}
-- import Web.Stripe
-- import Web.Stripe.ApplicationFeeRefund
--
-- main :: IO ()
-- main = do
--   let config = StripeConfig (StripeKey "secret_key")
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
    , UpdateApplicationFeeRefund
    , updateApplicationFeeRefund
    , GetApplicationFeeRefunds
    , getApplicationFeeRefunds
      -- * Types
    , FeeId                  (..)
    , RefundId               (..)
    , ApplicationFee         (..)
    , ApplicationFeeRefund   (..)
    , StripeList             (..)
    , EndingBefore           (..)
    , StartingAfter          (..)
    , Limit                  (..)
    , ExpandParams           (..)
    , MetaData               (..)
    , Amount                 (..)
    ) where

import           Web.Stripe.StripeRequest (Method (GET, POST), StripeHasParam,
                                           StripeRequest (..), StripeReturn,
                                           mkStripeRequest)
import           Web.Stripe.Util          ((</>))
import           Web.Stripe.Types         (Amount(..), ApplicationFee (..),
                                           ApplicationFeeRefund (..),
                                           EndingBefore(..), ExpandParams(..),
                                           FeeId (..), Limit(..), MetaData(..),
                                           RefundId (..), StartingAfter(..),
                                           StripeList (..))

------------------------------------------------------------------------------
-- | Create a new `ApplicationFeeRefund`
createApplicationFeeRefund
    :: FeeId        -- ^ The `FeeID` associated with the `ApplicationFee`
    -> StripeRequest CreateApplicationFeeRefund
createApplicationFeeRefund
    (FeeId feeid)
                = request
  where request = mkStripeRequest POST url params
        url     = "application_fees" </> feeid </> "refunds"
        params  = []

data CreateApplicationFeeRefund
type instance StripeReturn CreateApplicationFeeRefund = ApplicationFeeRefund
instance StripeHasParam CreateApplicationFeeRefund Amount
instance StripeHasParam CreateApplicationFeeRefund MetaData

------------------------------------------------------------------------------
-- | Retrieve an existing 'ApplicationFeeRefund'
getApplicationFeeRefund
    :: FeeId     -- ^ The `FeeID` associated with the `ApplicationFee`
    -> RefundId  -- ^ The `ReufndId` associated with the `ApplicationFeeRefund`
    -> StripeRequest GetApplicationFeeRefund
getApplicationFeeRefund
  (FeeId feeid)
  (RefundId refundid)
                = request
  where request = mkStripeRequest GET url params
        url     = "application_fees" </> feeid </> "refunds" </> refundid
        params  = []

data GetApplicationFeeRefund
type instance StripeReturn GetApplicationFeeRefund = ApplicationFeeRefund
instance StripeHasParam GetApplicationFeeRefund ExpandParams

------------------------------------------------------------------------------
-- | Update an `ApplicationFeeRefund` for a given Application `FeeId` and `RefundId`
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

data UpdateApplicationFeeRefund
type instance StripeReturn UpdateApplicationFeeRefund = ApplicationFeeRefund
instance StripeHasParam UpdateApplicationFeeRefund MetaData

------------------------------------------------------------------------------
-- | Retrieve a list of all 'ApplicationFeeRefund's for a given Application 'FeeId'
getApplicationFeeRefunds
    :: FeeId               -- ^ The `FeeID` associated with the application
    -> StripeRequest GetApplicationFeeRefunds
getApplicationFeeRefunds
   (FeeId feeid) = request
  where
    request = mkStripeRequest GET url params
    url     = "application_fees" </> feeid </> "refunds"
    params  = []

data GetApplicationFeeRefunds
type instance StripeReturn GetApplicationFeeRefunds = (StripeList ApplicationFeeRefund)
instance StripeHasParam GetApplicationFeeRefunds ExpandParams
instance StripeHasParam GetApplicationFeeRefunds (EndingBefore RefundId)
instance StripeHasParam GetApplicationFeeRefunds Limit
instance StripeHasParam GetApplicationFeeRefunds (StartingAfter RefundId)
