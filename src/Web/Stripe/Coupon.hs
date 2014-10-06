{-# LANGUAGE OverloadedStrings #-}
module Web.Stripe.Coupon
    ( -- * API
      createCoupon
    , getCoupon
    , getCoupons
    , deleteCoupon
      -- * Types
    , Duration
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

import           Web.Stripe.Client.Internal (Method (GET, POST, DELETE), Stripe,
                                             StripeRequest (..), callAPI,
                                             getParams, toText, (</>))
import           Web.Stripe.Types           (AmountOff (..), Coupon (..),
                                             CouponId (..), Currency (..),
                                             Duration, DurationInMonths (..),
                                             EndingBefore, Limit,
                                             MaxRedemptions (..),
                                             PercentOff (..), RedeemBy (..),
                                             StartingAfter,
                                             StripeDeleteResult (..),
                                             StripeList (..))

------------------------------------------------------------------------------
-- | 'Coupon' creation function
createCoupon
  :: Maybe CouponId
  -> Duration
  -> Maybe AmountOff
  -> Maybe Currency
  -> Maybe DurationInMonths
  -> Maybe MaxRedemptions
  -> Maybe PercentOff
  -> Maybe RedeemBy
  -> Stripe Coupon
createCoupon
    couponId
    duration
    amountOff
    currency
    durationInMonths
    maxRedemptions
    percentOff
    redeemBy = callAPI request
  where request = StripeRequest POST url params
        url     = "coupons"
        params  = getParams [
                    ("id", (\(CouponId x) -> x) `fmap` couponId )
                  , ("duration", toText `fmap` Just duration )
                  , ("amount_off", (\(AmountOff x) -> toText x) `fmap` amountOff )
                  , ("currency", (\(Currency x) -> x) `fmap` currency)
                  , ("duration_in_months", (\(DurationInMonths x) -> toText x) `fmap` durationInMonths )
                  , ("max_redemptions", (\(MaxRedemptions x) -> toText x) `fmap` maxRedemptions )
                  , ("percent_off", (\(PercentOff x) -> toText x) `fmap` percentOff )
                  , ("redeem_by", (\(RedeemBy x) -> toText x) `fmap` redeemBy )
                 ]

------------------------------------------------------------------------------
-- | Retrieve a 'Coupon' by 'CouponId'
getCoupon
    :: CouponId
    -> Stripe Coupon
getCoupon
    (CouponId couponId) = callAPI request
  where request = StripeRequest POST url params
        url     = "coupons" </> couponId
        params  = []

------------------------------------------------------------------------------
-- | Retrieve a list of 'Coupon's
getCoupons
    :: Maybe Limit            -- ^ Defaults to 10 if `Nothing` specified
    -> StartingAfter CouponId -- ^ Paginate starting after the following `CouponId`
    -> EndingBefore CouponId  -- ^ Paginate ending before the following `CouponId`
    -> Stripe (StripeList Coupon)
getCoupons
     limit
     startingAfter
     endingBefore
  = callAPI request
  where request = StripeRequest POST url params
        url     = "coupons"
        params  = getParams [
            ("limit", toText `fmap` limit )
          , ("starting_after", (\(CouponId x) -> x) `fmap` startingAfter)
          , ("ending_before", (\(CouponId x) -> x) `fmap` endingBefore)
          ]


------------------------------------------------------------------------------
-- | Delete a 'Coupon" by 'CouponId'
deleteCoupon
    :: CouponId
    -> Stripe StripeDeleteResult
deleteCoupon
    (CouponId couponId) = callAPI request
  where request = StripeRequest DELETE url params
        url     = "coupons" </> couponId
        params  = []

