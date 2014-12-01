{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
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
import           Web.Stripe.Types          (AmountOff (..), Coupon (..),
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
data CreateCoupon
type instance StripeReturn CreateCoupon = Coupon
instance StripeHasParam CreateCoupon AmountOff
instance StripeHasParam CreateCoupon Currency
instance StripeHasParam CreateCoupon DurationInMonths
instance StripeHasParam CreateCoupon MaxRedemptions
instance StripeHasParam CreateCoupon MetaData
instance StripeHasParam CreateCoupon PercentOff
instance StripeHasParam CreateCoupon RedeemBy
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
                      toStripeParam (Param ("id"::Text, name)) []
                    Nothing   -> []

------------------------------------------------------------------------------
-- | Retrieve `Coupon`
data GetCoupon
type instance StripeReturn GetCoupon = Coupon
getCoupon
    :: CouponId -- ^ `CouponId` of the `Coupon` to retrieve
    -> StripeRequest GetCoupon
getCoupon
    (CouponId couponid) = request
  where request = mkStripeRequest GET url params
        url     = "coupons" </> couponid
        params  = []

------------------------------------------------------------------------------
-- | Update `Coupon`
data UpdateCoupon
type instance StripeReturn UpdateCoupon = Coupon
instance StripeHasParam UpdateCoupon MetaData
updateCoupon
    :: CouponId -- ^ The `CoupondId` of the `Coupon` to update
    -> StripeRequest UpdateCoupon
updateCoupon
     (CouponId couponid)
                = request
  where request = mkStripeRequest POST url params
        url     = "coupons" </> couponid
        params  = []

------------------------------------------------------------------------------
-- | Delete `Coupon`
data DeleteCoupon
type instance StripeReturn DeleteCoupon = StripeDeleteResult
deleteCoupon
    :: CouponId -- ^ The `CoupondId` of the `Coupon` to delete
    -> StripeRequest DeleteCoupon
deleteCoupon
    (CouponId couponid) = request
  where request = mkStripeRequest DELETE url params
        url     = "coupons" </> couponid
        params  = []

------------------------------------------------------------------------------
-- | Retrieve a list of `Coupon`s
data GetCoupons
type instance StripeReturn GetCoupons = (StripeList Coupon)
instance StripeHasParam GetCoupons (EndingBefore CouponId)
instance StripeHasParam GetCoupons Limit
instance StripeHasParam GetCoupons (StartingAfter CouponId)
getCoupons
    :: StripeRequest GetCoupons
getCoupons
  = request
  where request = mkStripeRequest GET url params
        url     = "coupons"
        params  = []
