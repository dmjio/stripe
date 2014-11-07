{-# LANGUAGE OverloadedStrings #-}
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
      getApplicationFee
    , getApplicationFeeExpanded
    , getApplicationFees
    , getApplicationFeesExpanded
       -- * Types
    , ApplicationId  (..)
    , ApplicationFee (..)
    , FeeId          (..)
    , StripeList     (..)
    , EndingBefore
    , StartingAfter
    , Limit
    , ExpandParams
    , ConnectApp     (..)
    ) where

import           Web.Stripe.Client.Types    (Method (GET), StripeRequest (..))
import           Web.Stripe.Client.Util     (getParams, toText, (</>), toExpandable)
import           Web.Stripe.Types           (ApplicationFee (..),
                                             ApplicationId (..), ConnectApp (..),
                                             EndingBefore, FeeId (..),
                                             Limit, StartingAfter, ExpandParams,
                                             StripeList (..))

------------------------------------------------------------------------------
-- | 'ApplicationFee' retrieval
getApplicationFee
    :: FeeId        -- ^ The `FeeId` associated with the Application
    -> StripeRequest ApplicationFee
getApplicationFee
    feeid = getApplicationFeeExpanded feeid []

------------------------------------------------------------------------------
-- | 'ApplicationFee' retrieval with `ExpandParams`
getApplicationFeeExpanded
    :: FeeId        -- ^ The `FeeId` associated with the application
    -> ExpandParams -- ^ The `ExpandParams` for an `ApplicationFee`
    -> StripeRequest ApplicationFee
getApplicationFeeExpanded
    (FeeId feeid)
    expandParams = request
  where request = StripeRequest GET url params
        url     = "application_fees" </> feeid
        params  = toExpandable expandParams

------------------------------------------------------------------------------
-- | 'ApplicationFee's retrieval
getApplicationFees
    :: Maybe Limit         -- ^ Defaults to 10 if `Nothing` specified
    -> StartingAfter FeeId -- ^ Paginate starting after the following `FeeId`
    -> EndingBefore FeeId  -- ^ Paginate ending before the following `FeeId`
    -> StripeRequest (StripeList ApplicationFee)
getApplicationFees
    limit
    startingAfter
    endingBefore =
      getApplicationFeesExpanded
        limit startingAfter endingBefore []

------------------------------------------------------------------------------
-- | 'ApplicationFee's retrieval with `ExpandParams`
getApplicationFeesExpanded
    :: Maybe Limit         -- ^ Defaults to 10 if `Nothing` specified
    -> StartingAfter FeeId -- ^ Paginate starting after the following `FeeId`
    -> EndingBefore FeeId  -- ^ Paginate ending before the following `FeeId`
    -> ExpandParams        -- ^ The `ExpandParams` for `ApplicationFee`s
    -> StripeRequest (StripeList ApplicationFee)
getApplicationFeesExpanded
    limit
    startingAfter
    endingBefore
    expandParams = request
  where request = StripeRequest GET url params
        url     = "application_fees"
        params  = getParams [
            ("limit", toText `fmap` limit )
          , ("starting_after", (\(FeeId x) -> x) `fmap` startingAfter)
          , ("ending_before", (\(FeeId x) -> x) `fmap` endingBefore)
          ] ++ toExpandable expandParams
