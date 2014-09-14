{-# LANGUAGE OverloadedStrings #-}

module Web.Stripe.Coupon 
    ( -- * Coupon Types
      Coupon   (..)
    , CouponId (..)
     -- * API calls
    , createCoupon
    , getCoupon
    , deleteCoupon
    ) where

import           Control.Applicative
import           Web.Stripe.Client.Internal
import           Web.Stripe.Types

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
                    ("id", (\(CouponId x) -> x) <$> couponId )
                  , ("duration", toText <$> Just duration )
                  , ("amount_off", (\(AmountOff x) -> toText x) <$> amountOff )
                  , ("currency", (\(Currency x) -> x) <$> currency)
                  , ("duration_in_months", (\(DurationInMonths x) -> toText x) <$> durationInMonths )
                  , ("max_redemptions", (\(MaxRedemptions x) -> toText x) <$> maxRedemptions )
                  , ("percent_off", (\(PercentOff x) -> toText x) <$> percentOff )
                  , ("redeem_by", (\(RedeemBy x) -> toText x) <$> redeemBy )
                 ]

getCoupon
    :: CouponId
    -> Stripe Coupon
getCoupon
    (CouponId couponId) = callAPI request
  where request = StripeRequest POST url params
        url     = "coupons" </> couponId
        params  = []

deleteCoupon 
    :: CouponId
    -> Stripe StripeDeleteResult
deleteCoupon 
    (CouponId couponId) = callAPI request
  where request = StripeRequest DELETE url params
        url     = "coupons" </> couponId
        params  = []

