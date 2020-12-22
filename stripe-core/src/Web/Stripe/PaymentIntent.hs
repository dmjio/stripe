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
module Web.Stripe.PaymentIntent
    ( -- * API
      CreatePaymentIntent
    , createPaymentIntent
    , GetPaymentIntent
    , getPaymentIntent
    , UpdatePaymentIntent
    , updatePaymentIntent
    , ConfirmPaymentIntent
    , confirmPaymentIntent
    , CapturePaymentIntent
    , capturePaymentIntent
    , CancelPaymentIntent
    , cancelPaymentIntent
    , GetPaymentIntents
    , getPaymentIntents
      -- * Types
    , Amount       (..)
    , CardId       (..)
    , Charge       (..)
    , ChargeId     (..)
    , Currency     (..)
    , EndingBefore (..)
    , ExpandParams (..)
    , PaymentIntent       (..)
    , PaymentIntentId     (..)
    , PaymentMethodId     (..)
    , PaymentMethodTypes     (..)
    , PaymentMethodType     (..)
    , StripeList   (..)
    , Token (..)
    ) where

import           Web.Stripe.StripeRequest   (Method (GET, POST),
                                             StripeHasParam, StripeReturn,
                                             StripeRequest (..), toStripeParam, mkStripeRequest)
import           Web.Stripe.Util            ((</>))
import           Web.Stripe.Types           (Amount(..), Charge (..), CardId (..), ChargeId (..), Currency(..), CustomerId(..),
                                             EndingBefore(..), Limit(..),
                                             MetaData(..), PaymentIntent (..), PaymentMethodId (..), PaymentMethodTypes(..), PaymentMethodType(..),
                                             PaymentIntentId (..), ReceiptEmail(..),
                                             StartingAfter(..), ExpandParams(..),
                                             StripeList (..), Token (..))

------------------------------------------------------------------------------
-- | create a `PaymentIntent`
createPaymentIntent
    :: Amount
    -> Currency
    -> StripeRequest CreatePaymentIntent
createPaymentIntent
    amount
    currency    = request
  where request = mkStripeRequest POST url params
        url     = "payment_intents"
        params  = toStripeParam amount $
                  toStripeParam currency $
                  []

data CreatePaymentIntent
type instance StripeReturn CreatePaymentIntent = PaymentIntent
instance StripeHasParam CreatePaymentIntent CustomerId
instance StripeHasParam CreatePaymentIntent ReceiptEmail
instance StripeHasParam CreatePaymentIntent PaymentMethodTypes

------------------------------------------------------------------------------
-- | Retrieve a `PaymentIntent` by `ChargeId` and `PaymentIntentId`
getPaymentIntent
    :: PaymentIntentId -- ^ `PaymentIntentId` associated with the `PaymentIntent` to be retrieved
    -> StripeRequest GetPaymentIntent
getPaymentIntent
   (PaymentIntentId paymentIntentid) = request
   where request = mkStripeRequest GET url params
         url     = "payment_intents" </> paymentIntentid
         params  = []

data GetPaymentIntent
type instance StripeReturn GetPaymentIntent = PaymentIntent
instance StripeHasParam GetPaymentIntent ExpandParams

------------------------------------------------------------------------------
-- | Update a `PaymentIntent` by `ChargeId` and `PaymentIntentId`
updatePaymentIntent
    :: PaymentIntentId -- ^ `PaymentIntentId` associated with the `PaymentIntent` to be retrieved
    -> StripeRequest UpdatePaymentIntent
updatePaymentIntent
   (PaymentIntentId paymentIntentid)
                = request
  where request = mkStripeRequest POST url params
        url     = "payment_intents" </> paymentIntentid
        params  = []

data UpdatePaymentIntent
type instance StripeReturn UpdatePaymentIntent = PaymentIntent
instance StripeHasParam UpdatePaymentIntent MetaData
instance StripeHasParam UpdatePaymentIntent PaymentMethodId

confirmPaymentIntent
    :: PaymentIntentId
    -> StripeRequest ConfirmPaymentIntent
confirmPaymentIntent
    (PaymentIntentId paymentIntentid)
              = request
  where request = mkStripeRequest POST url params
        url     = "payment_intents" </> paymentIntentid </> "confirm"
        params  = []

data ConfirmPaymentIntent
type instance StripeReturn ConfirmPaymentIntent = PaymentIntent
instance StripeHasParam ConfirmPaymentIntent MetaData
instance StripeHasParam ConfirmPaymentIntent PaymentMethodId

capturePaymentIntent
    :: PaymentIntentId
    -> StripeRequest CapturePaymentIntent
capturePaymentIntent
    (PaymentIntentId paymentIntentid)
              = request
  where request = mkStripeRequest POST url params
        url     = "payment_intents" </> paymentIntentid </> "capture"
        params  = []

data CapturePaymentIntent
type instance StripeReturn CapturePaymentIntent = PaymentIntent
instance StripeHasParam CapturePaymentIntent MetaData

cancelPaymentIntent
    :: PaymentIntentId
    -> StripeRequest CancelPaymentIntent
cancelPaymentIntent
    (PaymentIntentId paymentIntentid)
              = request
  where request = mkStripeRequest POST url params
        url     = "payment_intents" </> paymentIntentid </> "cancel"
        params  = []

data CancelPaymentIntent
type instance StripeReturn CancelPaymentIntent = PaymentIntent
instance StripeHasParam CancelPaymentIntent MetaData

------------------------------------------------------------------------------
-- | Retrieve a list of PaymentIntents
getPaymentIntents
    :: StripeRequest GetPaymentIntents
getPaymentIntents
        = request
  where request = mkStripeRequest GET url params
        url     = "payment_intents"
        params  = []

data GetPaymentIntents
type instance StripeReturn GetPaymentIntents = StripeList PaymentIntent
instance StripeHasParam GetPaymentIntents ExpandParams
instance StripeHasParam GetPaymentIntents (EndingBefore PaymentIntentId)
instance StripeHasParam GetPaymentIntents Limit
instance StripeHasParam GetPaymentIntents (StartingAfter PaymentIntentId)
