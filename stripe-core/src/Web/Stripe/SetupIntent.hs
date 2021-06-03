{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
-------------------------------------------
-- |
-- Module      : Web.Stripe.SetupIntent
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Web.Stripe.SetupIntent
    ( -- * API
      CreateSetupIntent
    , createSetupIntent
    , GetSetupIntent
    , getSetupIntent
    , UpdateSetupIntent
    , updateSetupIntent
    , ConfirmSetupIntent
    , confirmSetupIntent
    , CancelSetupIntent
    , cancelSetupIntent
    , GetSetupIntents
    , getSetupIntents
      -- * Types
    , Amount       (..)
    , CardId       (..)
    , Charge       (..)
    , ChargeId     (..)
    , Currency     (..)
    , CustomerId   (..)
    , Description  (..)
    , EndingBefore (..)
    , ExpandParams (..)
    , SetupIntent       (..)
    , SetupIntentId     (..)
    , PaymentMethodId     (..)
    , PaymentMethodTypes  (..)
    , PaymentMethodType   (..)
    , SetupIntentUsage    (..)
    , Usage    (..)
    , StripeList   (..)
    , Token (..)
    ) where

import           Web.Stripe.StripeRequest   (Method (GET, POST),
                                             StripeHasParam, StripeReturn,
                                             StripeRequest (..), toStripeParam, mkStripeRequest)
import           Web.Stripe.Util            ((</>))
import           Web.Stripe.Types           (Amount(..), Charge (..), CardId (..), ChargeId (..), Currency(..), CustomerId(..),
                                             Description(..), EndingBefore(..), Limit(..),
                                             MetaData(..), SetupIntent (..), PaymentMethodId (..), PaymentMethodTypes(..), PaymentMethodType(..),
                                             SetupIntentId (..), ReceiptEmail(..),
                                             SetupIntentUsage(..), Usage (..), StartingAfter(..), ExpandParams(..),
                                             StripeList (..), Token (..))

------------------------------------------------------------------------------
-- | create a `SetupIntent`
createSetupIntent :: StripeRequest CreateSetupIntent
createSetupIntent = request
  where request = mkStripeRequest POST url params
        url     = "setup_intents"
        params  = []

data CreateSetupIntent
type instance StripeReturn CreateSetupIntent = SetupIntent
instance StripeHasParam CreateSetupIntent CustomerId
instance StripeHasParam CreateSetupIntent Description
instance StripeHasParam CreateSetupIntent PaymentMethodTypes
instance StripeHasParam CreateSetupIntent SetupIntentUsage

------------------------------------------------------------------------------
-- | Retrieve a `SetupIntent` by `ChargeId` and `SetupIntentId`
getSetupIntent
    :: SetupIntentId -- ^ `SetupIntentId` associated with the `SetupIntent` to be retrieved
    -> StripeRequest GetSetupIntent
getSetupIntent
   (SetupIntentId setupIntentid) = request
   where request = mkStripeRequest GET url params
         url     = "setup_intents" </> setupIntentid
         params  = []

data GetSetupIntent
type instance StripeReturn GetSetupIntent = SetupIntent
instance StripeHasParam GetSetupIntent ExpandParams

------------------------------------------------------------------------------
-- | Update a `SetupIntent` by `ChargeId` and `SetupIntentId`
updateSetupIntent
    :: SetupIntentId -- ^ `SetupIntentId` associated with the `SetupIntent` to be retrieved
    -> StripeRequest UpdateSetupIntent
updateSetupIntent
   (SetupIntentId setupIntentid)
                = request
  where request = mkStripeRequest POST url params
        url     = "setup_intents" </> setupIntentid
        params  = []

data UpdateSetupIntent
type instance StripeReturn UpdateSetupIntent = SetupIntent
instance StripeHasParam UpdateSetupIntent MetaData
instance StripeHasParam UpdateSetupIntent PaymentMethodId
instance StripeHasParam UpdateSetupIntent CustomerId
instance StripeHasParam UpdateSetupIntent Description
instance StripeHasParam UpdateSetupIntent PaymentMethodTypes

confirmSetupIntent
    :: SetupIntentId
    -> StripeRequest ConfirmSetupIntent
confirmSetupIntent
    (SetupIntentId setupIntentid)
              = request
  where request = mkStripeRequest POST url params
        url     = "setup_intents" </> setupIntentid </> "confirm"
        params  = []

data ConfirmSetupIntent
type instance StripeReturn ConfirmSetupIntent = SetupIntent
instance StripeHasParam ConfirmSetupIntent MetaData
instance StripeHasParam ConfirmSetupIntent PaymentMethodId

cancelSetupIntent
    :: SetupIntentId
    -> StripeRequest CancelSetupIntent
cancelSetupIntent
    (SetupIntentId setupIntentid)
              = request
  where request = mkStripeRequest POST url params
        url     = "setup_intents" </> setupIntentid </> "cancel"
        params  = []

data CancelSetupIntent
type instance StripeReturn CancelSetupIntent = SetupIntent

------------------------------------------------------------------------------
-- | Retrieve a list of SetupIntents
getSetupIntents
    :: StripeRequest GetSetupIntents
getSetupIntents
        = request
  where request = mkStripeRequest GET url params
        url     = "setup_intents"
        params  = []

data GetSetupIntents
type instance StripeReturn GetSetupIntents = StripeList SetupIntent
instance StripeHasParam GetSetupIntents ExpandParams
instance StripeHasParam GetSetupIntents (EndingBefore SetupIntentId)
instance StripeHasParam GetSetupIntents Limit
instance StripeHasParam GetSetupIntents (StartingAfter SetupIntentId)
