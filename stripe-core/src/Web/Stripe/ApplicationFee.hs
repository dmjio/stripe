{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
-------------------------------------------
-- |
-- Module      : Web.Stripe.AppplicationFee
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- < https:/\/\stripe.com/docs/api#application_fees >
--
-- @
-- import Web.Stripe
-- import Web.Stripe.ApplicationFee
--
-- main :: IO ()
-- main = do
--   let config = SecretKey "secret_key"
--   result <- stripe config $ getApplicationFee (FeeId "fee_4xtEGZhPNDEt3w")
--   case result of
--     Right applicationFee -> print applicationFee
--     Left stripeError     -> print stripeError
-- @
module Web.Stripe.ApplicationFee
    (  -- * API
      GetApplicationFee
    , getApplicationFee
    , getApplicationFeeExpanded
    , GetApplicationFees
    , getApplicationFees
    , getApplicationFeesExpanded
       -- * Types
    , ApplicationId  (..)
    , ApplicationFee (..)
    , ApplicationFeeId (..)
    , ChargeId       (..)
    , ConnectApp     (..)
    , Created        (..)
    , EndingBefore   (..)
    , FeeId          (..)
    , Limit          (..)
    , StartingAfter  (..)
    , StripeList     (..)
    , ExpandParams
    ) where
import           Web.Stripe.StripeRequest (Method (GET), StripeHasParam,
                                           StripeRequest (..), StripeReturn,
                                           mkStripeRequest)
import           Web.Stripe.Util          ((</>), toExpandable)
import           Web.Stripe.Types          (ApplicationFee (..),
                                            ApplicationFeeId (..),
                                            ApplicationId (..), ChargeId(..),
                                            ConnectApp (..), Created(..),
                                            EndingBefore(..), FeeId (..),
                                            Limit(..), StartingAfter(..),
                                            ExpandParams,
                                            StripeList (..))

------------------------------------------------------------------------------
-- | 'ApplicationFee' retrieval
data GetApplicationFee
type instance StripeReturn GetApplicationFee = ApplicationFee
getApplicationFee
    :: FeeId        -- ^ The `FeeId` associated with the Application
    -> StripeRequest GetApplicationFee
getApplicationFee
    feeid = getApplicationFeeExpanded feeid []

------------------------------------------------------------------------------
-- | 'ApplicationFee' retrieval with `ExpandParams`
getApplicationFeeExpanded
    :: FeeId        -- ^ The `FeeId` associated with the application
    -> ExpandParams -- ^ The `ExpandParams` for an `ApplicationFee`
    -> StripeRequest GetApplicationFee
getApplicationFeeExpanded
    (FeeId feeid)
    expandParams = request
  where request = mkStripeRequest GET url params
        url     = "application_fees" </> feeid
        params  = toExpandable expandParams

------------------------------------------------------------------------------
-- | 'ApplicationFee's retrieval
data GetApplicationFees
type instance StripeReturn GetApplicationFees =  (StripeList ApplicationFee)
instance StripeHasParam GetApplicationFees ChargeId
instance StripeHasParam GetApplicationFees Created
instance StripeHasParam GetApplicationFees (EndingBefore ApplicationFeeId)
instance StripeHasParam GetApplicationFees Limit
instance StripeHasParam GetApplicationFees (StartingAfter ApplicationFeeId)
getApplicationFees
    :: StripeRequest GetApplicationFees
getApplicationFees
    = getApplicationFeesExpanded []

------------------------------------------------------------------------------
-- | 'ApplicationFee's retrieval with `ExpandParams`
getApplicationFeesExpanded
    :: ExpandParams        -- ^ The `ExpandParams` for `ApplicationFee`s
    -> StripeRequest GetApplicationFees
getApplicationFeesExpanded
   expandParams = request
  where request = mkStripeRequest GET url params
        url     = "application_fees"
        params  = toExpandable expandParams
