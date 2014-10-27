{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Web.Stripe.ApplicationFee
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
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

import           Web.Stripe.Client.Internal (Method (GET), Stripe,
                                             StripeRequest (..), callAPI,
                                             getParams, toText, (</>), toExpandable)
import           Web.Stripe.Types           (ApplicationFee (..),
                                             ApplicationId (..), ConnectApp (..),
                                             EndingBefore, FeeId (..),
                                             Limit, StartingAfter, ExpandParams,
                                             StripeList (..))

------------------------------------------------------------------------------
-- | 'ApplicationFee' retrieval
getApplicationFee
    :: FeeId        -- ^ The `FeeId` associated with the Application
    -> Stripe ApplicationFee
getApplicationFee
    feeid = getApplicationFeeExpanded feeid []

------------------------------------------------------------------------------
-- | 'ApplicationFee' retrieval with `ExpandParams`
getApplicationFeeExpanded
    :: FeeId        -- ^ The `FeeId` associated with the application
    -> ExpandParams -- ^ The `ExpandParams` for an `ApplicationFee`
    -> Stripe ApplicationFee
getApplicationFeeExpanded
    (FeeId feeid)
    expandParams = callAPI request
  where request = StripeRequest GET url params
        url     = "application_fees" </> feeid
        params  = toExpandable expandParams

------------------------------------------------------------------------------
-- | 'ApplicationFee's retrieval
getApplicationFees
    :: Maybe Limit         -- ^ Defaults to 10 if `Nothing` specified
    -> StartingAfter FeeId -- ^ Paginate starting after the following `FeeId`
    -> EndingBefore FeeId  -- ^ Paginate ending before the following `FeeId`
    -> Stripe (StripeList ApplicationFee)
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
    -> Stripe (StripeList ApplicationFee)
getApplicationFeesExpanded
    limit
    startingAfter
    endingBefore
    expandParams = callAPI request
  where request = StripeRequest GET url params
        url     = "application_fees"
        params  = getParams [
            ("limit", toText `fmap` limit )
          , ("starting_after", (\(FeeId x) -> x) `fmap` startingAfter)
          , ("ending_before", (\(FeeId x) -> x) `fmap` endingBefore)
          ] ++ toExpandable expandParams


