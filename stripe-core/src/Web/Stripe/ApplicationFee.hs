{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
-------------------------------------------
-- |
-- Module      : Web.Stripe.AppplicationFee
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : code@dmj.io
-- Stability   : experimental
-- Portability : POSIX
--
-- < https:/\/\stripe.com/docs/api#application_fees >
--
-- @
-- {-\# LANGUAGE OverloadedStrings \#-}
-- import Web.Stripe
-- import Web.Stripe.ApplicationFee
--
-- main :: IO ()
-- main = do
--   let config = StripeConfig (StripeKey "secret_key")
--   result <- stripe config $ getApplicationFee (FeeId "fee_4xtEGZhPNDEt3w")
--   case result of
--     Right applicationFee -> print applicationFee
--     Left stripeError     -> print stripeError
-- @
module Web.Stripe.ApplicationFee
    (  -- * API
      GetApplicationFee
    , getApplicationFee
    , GetApplicationFees
    , getApplicationFees
       -- * Types
    , ApplicationId    (..)
    , ApplicationFee   (..)
    , ApplicationFeeId (..)
    , ChargeId         (..)
    , ConnectApp       (..)
    , Created          (..)
    , EndingBefore     (..)
    , FeeId            (..)
    , Limit            (..)
    , StartingAfter    (..)
    , StripeList       (..)
    , ExpandParams     (..)
    ) where
import           Web.Stripe.StripeRequest (Method (GET), StripeHasParam,
                                           StripeRequest (..), StripeReturn,
                                           mkStripeRequest)
import           Web.Stripe.Util          ((</>))
import           Web.Stripe.Types         (ApplicationFee (..),
                                           ApplicationFeeId (..),
                                           ApplicationId (..), ChargeId(..),
                                           ConnectApp (..), Created(..),
                                           EndingBefore(..), ExpandParams(..),
                                           FeeId (..), Limit(..),
                                           StartingAfter(..), ExpandParams(..),
                                           StripeList (..))

------------------------------------------------------------------------------
-- | 'ApplicationFee' retrieval
getApplicationFee
    :: FeeId        -- ^ The `FeeId` associated with the Application
    -> StripeRequest GetApplicationFee
getApplicationFee
    (FeeId feeid) = request
  where request = mkStripeRequest GET url params
        url     = "application_fees" </> feeid
        params  = []

data GetApplicationFee
type instance StripeReturn GetApplicationFee = ApplicationFee
instance StripeHasParam GetApplicationFee ExpandParams

------------------------------------------------------------------------------
-- | 'ApplicationFee's retrieval
getApplicationFees
    :: StripeRequest GetApplicationFees
getApplicationFees
    = request
  where request = mkStripeRequest GET url params
        url     = "application_fees"
        params  = []

data GetApplicationFees
type instance StripeReturn GetApplicationFees =  (StripeList ApplicationFee)
instance StripeHasParam GetApplicationFees ExpandParams
instance StripeHasParam GetApplicationFees ChargeId
instance StripeHasParam GetApplicationFees Created
instance StripeHasParam GetApplicationFees (EndingBefore ApplicationFeeId)
instance StripeHasParam GetApplicationFees Limit
instance StripeHasParam GetApplicationFees (StartingAfter ApplicationFeeId)
