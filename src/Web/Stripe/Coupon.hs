{-# LANGUAGE OverloadedStrings #-}
module Web.Stripe.Coupon where

import Web.Stripe.Types
import Web.Stripe.Util  
import Web.Stripe.Client.Internal
import Control.Applicative

createCoupon :: Maybe CouponId -> 
                Duration ->
                Maybe AmountOff ->
                Maybe Currency ->
                Maybe DurationInMonths ->
                Maybe MaxRedemptions ->
                Maybe PercentOff ->
                Maybe RedeemBy ->
                Stripe Coupon
createCoupon couponId duration 
amountOff currency durationInMonths 
maxRedemptions percentOff redeemBy
    = callAPI request
  where request = StripeRequest POST url params
        url     = "coupons"
        params  = [(k,v) | (k, Just v) <- [ 
                    ("id", (\(CouponId x) -> toBS x) <$> couponId )
                  , ("duration", Just $ toBS duration )
                  , ("amount_off", (\(AmountOff x) -> toBS x) <$> amountOff )
                  , ("currency", (\(Currency x) -> T.encodeUtf8 x) <$> currency )
                  , ("duration_in_months", (\(DurationInMonths x) -> toBS x) <$> durationInMonths )
                  , ("max_redemptions", (\(MaxRedemptions x) -> toBS x) <$> maxRedemptions )
                  , ("percent_off", (\(PercentOff x) -> toBS x) <$> percentOff )
                  , ("redeem_by", (\(RedeemBy x) -> toBS x) <$> redeemBy )
                  ]
                 ]

getCoupon :: CouponId -> Stripe Coupon
getCoupon (CouponId couponId) = callAPI request
  where request = StripeRequest POST url params
        url     = "coupons/" <> couponId
        params  = []

deleteCoupon :: CouponId -> Stripe StripeDeleteResult
deleteCoupon (CouponId couponId) = callAPI request
  where request = StripeRequest DELETE url params
        url     = "coupons/" <> couponId
        params  = []



