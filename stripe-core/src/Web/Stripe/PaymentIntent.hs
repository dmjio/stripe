{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
-------------------------------------------
-- |
-- Module      : Web.Stripe.PaymentIntent
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- < https:/\/\stripe.com/docs/api#refunds >
--
-- @
-- {-\# LANGUAGE OverloadedStrings \#-}
-- import Web.Stripe
-- import Web.Stripe.Customer
-- import Web.Stripe.Charge
-- import Web.Stripe.PaymentIntent
--
-- main :: IO ()
-- main = do
--   let config = StripeConfig (StripeKey "secret_key")
--       credit = CardNumber "4242424242424242"
--       em  = ExpMonth 12
--       ey  = ExpYear 2015
--       cvc = CVC "123"
--       cardinfo = (mkNewCard credit em ey) { newCardCVC = Just cvc }
--   result <- stripe config $ createCustomer -&- cardinfo
--   case result of
--     (Left stripeError) -> print stripeError
--     (Right (Customer { customerId = cid })) -> do
--       result <- stripe config $ createCharge (Amount 100) USD -&- cid
--       case result of
--         (Left stripeError) -> print stripeError
--         (Right (Charge { chargeId   = chid })) -> do
--           result <- stripe config $ createPaymentIntent chid
--           case result of
--             (Left stripeError) -> print stripeError
--             (Right refund)     -> print refund
-- @
module Web.Stripe.PaymentIntent
    ( -- * API
      CreatePaymentIntent
    , createPaymentIntent
    , GetPaymentIntent
    , getPaymentIntent
    , GetPaymentIntents
    , getPaymentIntents
    , UpdatePaymentIntent
    , updatePaymentIntent
      -- * Types
    , Amount       (..)
    , Charge       (..)
    , ChargeId     (..)
    , EndingBefore (..)
    , ExpandParams (..)
    , PaymentIntent       (..)
    , PaymentIntentApplicationFee(..)
    , PaymentIntentReason (..)
    , PaymentIntentId     (..)
    , StripeList   (..)
    ) where

import           Web.Stripe.StripeRequest   (Method (GET, POST),
                                             StripeHasParam, StripeReturn,
                                             StripeRequest (..), mkStripeRequest)
import           Web.Stripe.Util            ((</>))
import           Web.Stripe.Types           (Amount(..), Charge (..), ChargeId (..),
                                             EndingBefore(..), Limit(..),
                                             MetaData(..), PaymentIntent (..),
                                             PaymentIntentApplicationFee(..),
                                             PaymentIntentId (..), PaymentIntentReason(..),
                                             StartingAfter(..), ExpandParams(..),
                                             StripeList (..))
import           Web.Stripe.Types.Util      (getChargeId)

------------------------------------------------------------------------------
-- | create a `PaymentIntent`
createPaymentIntent
    :: ChargeId -- ^ `ChargeId` associated with the `Charge` to be refunded
    -> StripeRequest CreatePaymentIntent
createPaymentIntent
    chargeid    = request
  where request = mkStripeRequest POST url params
        url     = "charges" </> getChargeId chargeid </> "refunds"
        params  = []

data CreatePaymentIntent
type instance StripeReturn CreatePaymentIntent = PaymentIntent
instance StripeHasParam CreatePaymentIntent Amount
instance StripeHasParam CreatePaymentIntent PaymentIntentApplicationFee
instance StripeHasParam CreatePaymentIntent PaymentIntentReason
instance StripeHasParam CreatePaymentIntent MetaData

------------------------------------------------------------------------------
-- | Retrieve a `PaymentIntent` by `ChargeId` and `PaymentIntentId`
getPaymentIntent
    :: ChargeId -- ^ `ChargeId` associated with the `PaymentIntent` to be retrieved
    -> PaymentIntentId -- ^ `PaymentIntentId` associated with the `PaymentIntent` to be retrieved
    -> StripeRequest GetPaymentIntent
getPaymentIntent
   chargeid
   (PaymentIntentId refundid) = request
   where request = mkStripeRequest GET url params
         url     = "charges" </> getChargeId chargeid </> "refunds" </> refundid
         params  = []

data GetPaymentIntent
type instance StripeReturn GetPaymentIntent = PaymentIntent
instance StripeHasParam GetPaymentIntent ExpandParams

------------------------------------------------------------------------------
-- | Update a `PaymentIntent` by `ChargeId` and `PaymentIntentId`
updatePaymentIntent
    :: ChargeId -- ^ `ChargeId` associated with the `Charge` to be updated
    -> PaymentIntentId -- ^ `PaymentIntentId` associated with the `PaymentIntent` to be retrieved
    -> StripeRequest UpdatePaymentIntent
updatePaymentIntent
   chargeid
   (PaymentIntentId refid)
                = request
  where request = mkStripeRequest POST url params
        url     = "charges" </> getChargeId chargeid  </> "refunds" </> refid
        params  = []

data UpdatePaymentIntent
type instance StripeReturn UpdatePaymentIntent = PaymentIntent
instance StripeHasParam UpdatePaymentIntent MetaData

------------------------------------------------------------------------------
-- | Retrieve a lot of PaymentIntents by `ChargeId`
getPaymentIntents
    :: ChargeId               -- ^ `ChargeId` associated with the `PaymentIntents` to get
    -> StripeRequest GetPaymentIntents
getPaymentIntents
  chargeid = request
  where request = mkStripeRequest GET url params
        url     = "charges" </> getChargeId chargeid </> "refunds"
        params  = []

data GetPaymentIntents
type instance StripeReturn GetPaymentIntents = StripeList PaymentIntent
instance StripeHasParam GetPaymentIntents ExpandParams
instance StripeHasParam GetPaymentIntents (EndingBefore PaymentIntentId)
instance StripeHasParam GetPaymentIntents Limit
instance StripeHasParam GetPaymentIntents (StartingAfter PaymentIntentId)
