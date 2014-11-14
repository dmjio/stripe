{-# LANGUAGE OverloadedStrings #-}
-------------------------------------------
-- |
-- Module      : Web.Stripe.Coupon
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- < https:/\/\stripe.com/docs/api#coupons >
--
-- @
-- import Web.Stripe
-- import Web.Stripe.Coupon
--
-- main :: IO ()
-- main = do
--   let config = SecretKey "secret_key"
--   result <- createCoupon
--            (Just $ CouponId "$1 Off!")
--            Once
--            (Just $ AmountOff 1)
--            (Just USD)
--            Nothing
--            Nothing
--            Nothing
--            Nothing
--            []
--   case result of
--     Right coupon      -> print coupon
--     Left  stripeError -> print stripeError
-- @
module Web.Stripe.Coupon
    ( -- * API
      createCoupon
    , getCoupon
    , getCoupons
    , updateCoupon
    , deleteCoupon
      -- * Types
    , Duration           (..)
    , AmountOff          (..)
    , CouponId           (..)
    , Coupon             (..)
    , Currency           (..)
    , DurationInMonths   (..)
    , MaxRedemptions     (..)
    , PercentOff         (..)
    , RedeemBy           (..)
    , StripeList         (..)
    , StripeDeleteResult (..)
    ) where

import           Web.Stripe.Client.Types    (Method (POST, DELETE, GET),
                                             StripeRequest (..), mkStripeRequest)
import           Web.Stripe.Client.Util     (toMetaData,getParams, toText, (</>),
                                             toTextLower)
import           Web.Stripe.Types           (AmountOff (..), Coupon (..),
                                             CouponId (..), Currency (..),
                                             Duration(..), DurationInMonths (..),
                                             EndingBefore, Limit, MetaData,
                                             MaxRedemptions (..),
                                             PercentOff (..), RedeemBy (..),
                                             StartingAfter,
                                             StripeDeleteResult (..),
                                             StripeList (..))

------------------------------------------------------------------------------
-- | `Coupon` creation
createCoupon
  :: Maybe CouponId         -- ^  Name of the `Coupon`
  -> Duration               -- ^ `Duration` of the `Coupon`
  -> Maybe AmountOff        -- ^ `AmountOff` of the `Coupon`
  -> Maybe Currency         -- ^ `Currency` of the `Coupon`
  -> Maybe DurationInMonths -- ^ `DurationInMonths` of the `Coupon`
  -> Maybe MaxRedemptions   -- ^ `MaxRedemptions` of the `Coupon`
  -> Maybe PercentOff       -- ^ `PercentOff` of the `Coupon`
  -> Maybe RedeemBy         -- ^ `RedeemBy` date of the `Coupon`
  -> MetaData               -- ^ `MetaData` of the `Coupon`
  -> StripeRequest Coupon
createCoupon
    couponid
    duration
    amountOff
    currency
    durationInMonths
    maxRedemptions
    percentOff
    redeemBy
    metadata   = request
  where request = mkStripeRequest POST url params
        url     = "coupons"
        params  = toMetaData metadata ++ getParams [
                    ("id", (\(CouponId x) -> x) `fmap` couponid )
                  , ("duration", toText `fmap` Just duration )
                  , ("amount_off", (\(AmountOff x) -> toText x) `fmap` amountOff )
                  , ("currency", toTextLower `fmap` currency)
                  , ("duration_in_months", (\(DurationInMonths x) -> toText x) `fmap` durationInMonths )
                  , ("max_redemptions", (\(MaxRedemptions x) -> toText x) `fmap` maxRedemptions )
                  , ("percent_off", (\(PercentOff x) -> toText x) `fmap` percentOff )
                  , ("redeem_by", (\(RedeemBy x) -> toText x) `fmap` redeemBy )
                 ]

------------------------------------------------------------------------------
-- | Retrieve a `Coupon` by `CouponId`
getCoupon
    :: CouponId -- ^ `CouponId` of the `Coupon` to retrieve
    -> StripeRequest Coupon
getCoupon
    (CouponId couponid) = request
  where request = mkStripeRequest GET url params
        url     = "coupons" </> couponid
        params  = []

------------------------------------------------------------------------------
-- | Retrieve a list of 'Coupon's
getCoupons
    :: Maybe Limit            -- ^ Defaults to 10 if `Nothing` specified
    -> StartingAfter CouponId -- ^ Paginate starting after the following `CouponId`
    -> EndingBefore CouponId  -- ^ Paginate ending before the following `CouponId`
    -> StripeRequest (StripeList Coupon)
getCoupons
     limit
     startingAfter
     endingBefore
  = request
  where request = mkStripeRequest GET url params
        url     = "coupons"
        params  = getParams [
            ("limit", toText `fmap` limit )
          , ("starting_after", (\(CouponId x) -> x) `fmap` startingAfter)
          , ("ending_before", (\(CouponId x) -> x) `fmap` endingBefore)
          ]

------------------------------------------------------------------------------
-- | Update 'Coupon'
updateCoupon
    :: CouponId -- ^ The `CoupondId` of the `Coupon` to update
    -> MetaData -- ^ The `MetaData` for the `Coupon`
    -> StripeRequest Coupon
updateCoupon
     (CouponId couponid)
     metadata   = request
  where request = mkStripeRequest POST url params
        url     = "coupons" </> couponid
        params  = toMetaData metadata

------------------------------------------------------------------------------
-- | Delete 'Coupon" by 'CouponId'
deleteCoupon
    :: CouponId -- ^ The `CoupondId` of the `Coupon` to update
    -> StripeRequest StripeDeleteResult
deleteCoupon
    (CouponId couponid) = request
  where request = mkStripeRequest DELETE url params
        url     = "coupons" </> couponid
        params  = []
