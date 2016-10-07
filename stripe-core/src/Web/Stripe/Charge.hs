{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
-------------------------------------------
-- |
-- Module      : Web.Stripe.Charge
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- < https:/\/\stripe.com/docs/api#charges >
--
-- @
-- {-\# LANGUAGE OverloadedStrings \#-}
-- import Web.Stripe
-- import Web.Stripe.Customer
-- import Web.Stripe.Charge
--
-- main :: IO ()
-- main = do
--   let config = StripeConfig (StripeKey "secret_key")
--       credit = CardNumber "4242424242424242"
--       em  = ExpMonth 12
--       ey  = ExpYear 2015
--       cvc = CVC "123"
--       cardinfo = (newCard credit em ey) { newCardCVC = Just cvc }
--   result <- stripe config createCustomer
--                              -&- cardinfo
--   case result of
--     (Left stripeError) -> print stripeError
--     (Customer { customerId = cid }) ->
--       do result <- stripe config $ createCharge (Amount 100) USD
--                                      -&- cid
--          case result of
--            Left  stripeError -> print stripeError
--            Right charge      -> print charge
-- @
module Web.Stripe.Charge
    ( -- * API
      ---- * Create Charges
      CreateCharge
    , createCharge
      ---- * Get Charge(s)
    , GetCharge
    , getCharge
    , GetCharges
    , getCharges
      ---- * Update Charge
    , UpdateCharge
    , updateCharge
      ---- * Capture Charge
    , CaptureCharge
    , captureCharge
      -- * Types
    , Amount        (..)
    , ApplicationFeeAmount(..)
    , CardNumber    (..)
    , Capture       (..)
    , Charge        (..)
    , ChargeId      (..)
    , Created       (..)
    , Currency      (..)
    , CustomerId    (..)
    , Customer      (..)
    , CVC           (..)
    , Description   (..)
    , Email         (..)
    , EndingBefore  (..)
    , ExpandParams  (..)
    , ExpMonth      (..)
    , ExpYear       (..)
    , Limit         (..)
    , MetaData      (..)
    , NewCard       (..)
    , ReceiptEmail  (..)
    , StartingAfter (..)
    , StatementDescription (..)
    , StripeList    (..)
    , TokenId       (..)
    ) where

import           Web.Stripe.StripeRequest   (Method (GET, POST),
                                             StripeHasParam, ToStripeParam(..),
                                             StripeRequest (..), StripeReturn,
                                             mkStripeRequest)
import           Web.Stripe.Util            ((</>))
import           Web.Stripe.Types           (Amount(..), ApplicationFeeAmount(..),
                                             CVC (..),
                                             Capture(..),
                                             CardNumber (..), Charge (..),
                                             ChargeId (..), Created(..),
                                             Currency (..), Customer(..),
                                             CustomerId (..), Description(..),
                                             EndingBefore(..), ExpMonth (..),
                                             ExpYear (..), Limit(..), MetaData(..),
                                             NewCard(..), Email (..),
                                             StartingAfter(..),
                                             ReceiptEmail(..),
                                             StatementDescription(..),
                                             ExpandParams(..),
                                             StripeList (..), TokenId (..))
import           Web.Stripe.Types.Util      (getChargeId)

------------------------------------------------------------------------------
-- | Create a `Charge`
createCharge
    :: Amount   -- ^ `Amount` to charge
    -> Currency -- ^ `Currency` for charge
    -> StripeRequest CreateCharge
createCharge
    amount
    currency = request
  where request = mkStripeRequest POST url params
        url     = "charges"
        params  = toStripeParam amount   $
                  toStripeParam currency $
                  []

data CreateCharge
type instance StripeReturn CreateCharge = Charge
instance StripeHasParam CreateCharge ExpandParams
instance StripeHasParam CreateCharge CustomerId
instance StripeHasParam CreateCharge NewCard
instance StripeHasParam CreateCharge TokenId
instance StripeHasParam CreateCharge Description
instance StripeHasParam CreateCharge MetaData
instance StripeHasParam CreateCharge Capture
instance StripeHasParam CreateCharge StatementDescription
instance StripeHasParam CreateCharge ReceiptEmail
instance StripeHasParam CreateCharge ApplicationFeeAmount

------------------------------------------------------------------------------
-- | Retrieve a `Charge` by `ChargeId`
getCharge
    :: ChargeId -- ^ The `Charge` to retrive
    -> StripeRequest GetCharge
getCharge
    chargeid    = request
  where request = mkStripeRequest GET url params
        url     = "charges" </> getChargeId chargeid
        params  = []

data GetCharge
type instance StripeReturn GetCharge = Charge
instance StripeHasParam GetCharge ExpandParams

------------------------------------------------------------------------------
-- | A `Charge` to be updated
updateCharge
    :: ChargeId    -- ^ The `Charge` to update
    -> StripeRequest UpdateCharge
updateCharge
    chargeid    = request
  where request = mkStripeRequest POST url params
        url     = "charges" </> getChargeId chargeid
        params  = []

data UpdateCharge
type instance StripeReturn UpdateCharge = Charge
instance StripeHasParam UpdateCharge Description
instance StripeHasParam UpdateCharge MetaData

------------------------------------------------------------------------------
-- | a `Charge` to be captured
captureCharge
    :: ChargeId     -- ^ The `ChargeId` of the `Charge` to capture
    -> StripeRequest CaptureCharge
captureCharge
    chargeid     = request
  where request  = mkStripeRequest POST url params
        url      = "charges" </> getChargeId chargeid </> "capture"
        params   = []

data CaptureCharge
type instance StripeReturn CaptureCharge = Charge
instance StripeHasParam CaptureCharge Amount
instance StripeHasParam CaptureCharge ReceiptEmail

------------------------------------------------------------------------------
-- | Retrieve all `Charge`s
getCharges
    :: StripeRequest GetCharges
getCharges = request
  where request = mkStripeRequest GET url params
        url     = "charges"
        params  = []

data GetCharges
type instance StripeReturn GetCharges = StripeList Charge
instance StripeHasParam GetCharges ExpandParams
instance StripeHasParam GetCharges Created
instance StripeHasParam GetCharges CustomerId
instance StripeHasParam GetCharges (EndingBefore ChargeId)
instance StripeHasParam GetCharges Limit
instance StripeHasParam GetCharges (StartingAfter ChargeId)
