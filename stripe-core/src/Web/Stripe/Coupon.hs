{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
-------------------------------------------
-- |
-- Module      : Web.Stripe.Coupon
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : code@dmj.io
-- Stability   : experimental
-- Portability : POSIX
--
-- < https:/\/\stripe.com/docs/api#coupons >
--
-- @
-- {-\# LANGUAGE OverloadedStrings \#-}
-- import Web.Stripe
-- import Web.Stripe.Coupon
--
-- main :: IO ()
-- main = do
--   let config = StripeConfig (StripeKey "secret_key")
--   result <- stripe config $
--              createCoupon (Just $ CouponId "$1 Off!") Once
--               -&- (AmountOff 1)
--               -&- USD
--   case result of
--     Right coupon      -> print coupon
--     Left  stripeError -> print stripeError
-- @
module Web.Stripe.Coupon
    ( -- * API
      CreateCoupon
    , createCoupon
    , GetCoupon
    , getCoupon
    , UpdateCoupon
    , updateCoupon
    , DeleteCoupon
    , deleteCoupon
    , GetCoupons
    , getCoupons
      -- * Types
    , AmountOff          (..)
    , Coupon             (..)
    , CouponId           (..)
    , Currency           (..)
    , Duration           (..)
    , DurationInMonths   (..)
    , EndingBefore       (..)
    , Limit              (..)
    , MaxRedemptions     (..)
    , MetaData           (..)
    , PercentOff         (..)
    , RedeemBy           (..)
    , StartingAfter      (..)
    , StripeDeleteResult (..)
    , StripeList         (..)
    ) where

import           Data.Text                (Text)
import           Web.Stripe.StripeRequest (Method (GET, POST, DELETE), Param(..),
                                           StripeHasParam, StripeRequest (..),
                                           StripeReturn, ToStripeParam(..),
                                           mkStripeRequest)
import           Web.Stripe.Util          ((</>))
import           Web.Stripe.Types         (AmountOff (..), Coupon (..),
                                           CouponId (..), Currency (..),
                                           Duration(..), DurationInMonths (..),
                                           EndingBefore(..), Limit(..),
                                           MaxRedemptions (..), MetaData(..),
                                           PercentOff (..), RedeemBy (..),
                                           StartingAfter(..),
                                           StripeDeleteResult (..),
                                           StripeList (..))

------------------------------------------------------------------------------
-- | Create `Coupon`
createCoupon
  :: Maybe CouponId         -- ^  Name of the `Coupon`
  -> Duration               -- ^ `Duration` of the `Coupon`
  -> StripeRequest CreateCoupon
createCoupon
    mcouponid
    duration    = request
  where request = mkStripeRequest POST url params
        url     = "coupons"
        params  = case mcouponid of
                    Just (CouponId name) ->
                      toStripeParam (Param ("id"::Text, name)) $
                      toStripeParam duration                   $
                      []
                    Nothing   -> []

data CreateCoupon
type instance StripeReturn CreateCoupon = Coupon
instance StripeHasParam CreateCoupon AmountOff
instance StripeHasParam CreateCoupon Currency
instance StripeHasParam CreateCoupon DurationInMonths
instance StripeHasParam CreateCoupon MaxRedemptions
instance StripeHasParam CreateCoupon MetaData
instance StripeHasParam CreateCoupon PercentOff
instance StripeHasParam CreateCoupon RedeemBy

------------------------------------------------------------------------------
-- | Retrieve `Coupon`
getCoupon
    :: CouponId -- ^ `CouponId` of the `Coupon` to retrieve
    -> StripeRequest GetCoupon
getCoupon
    (CouponId couponid) = request
  where request = mkStripeRequest GET url params
        url     = "coupons" </> couponid
        params  = []

data GetCoupon
type instance StripeReturn GetCoupon = Coupon

------------------------------------------------------------------------------
-- | Update `Coupon`
updateCoupon
    :: CouponId -- ^ The `CoupondId` of the `Coupon` to update
    -> StripeRequest UpdateCoupon
updateCoupon
     (CouponId couponid)
                = request
  where request = mkStripeRequest POST url params
        url     = "coupons" </> couponid
        params  = []

data UpdateCoupon
type instance StripeReturn UpdateCoupon = Coupon
instance StripeHasParam UpdateCoupon MetaData

------------------------------------------------------------------------------
-- | Delete `Coupon`
deleteCoupon
    :: CouponId -- ^ The `CoupondId` of the `Coupon` to delete
    -> StripeRequest DeleteCoupon
deleteCoupon
    (CouponId couponid) = request
  where request = mkStripeRequest DELETE url params
        url     = "coupons" </> couponid
        params  = []

data DeleteCoupon
type instance StripeReturn DeleteCoupon = StripeDeleteResult

------------------------------------------------------------------------------
-- | Retrieve a list of `Coupon`s
getCoupons
    :: StripeRequest GetCoupons
getCoupons
  = request
  where request = mkStripeRequest GET url params
        url     = "coupons"
        params  = []

data GetCoupons
type instance StripeReturn GetCoupons = (StripeList Coupon)
instance StripeHasParam GetCoupons (EndingBefore CouponId)
instance StripeHasParam GetCoupons Limit
instance StripeHasParam GetCoupons (StartingAfter CouponId)
