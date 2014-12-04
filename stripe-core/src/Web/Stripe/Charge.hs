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
-- import Web.Stripe
-- import Web.Stripe.Customer
-- import Web.Stripe.Charge
--
-- main :: IO ()
-- main = do
--   let config = SecretKey "secret_key"
--       credit = CardNumber "4242424242424242"
--       em  = ExpMonth 12
--       ey  = ExpYear 2015
--       cvc = CVC "123"
--   result <- stripe config $ do
--         Customer { customerId = cid } <- createCustomerByCard cn em ey cvc
--         charge <- chargeCustomer cid USD 100 Nothing
--         return charge
--   case result of
--     Right charge      -> print charge
--     Left  stripeError -> print stripeError
-- @
module Web.Stripe.Charge
    ( -- * API
      ---- * Create Charges
      CreateCharge
    , createCharge
      ---- * Get Charge(s)
    , GetCharge
    , getCharge
    , getChargeExpandable
    , GetCharges
    , getCharges
    , getChargesExpandable
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
import           Web.Stripe.Util            ((</>), toExpandable)
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
                                             StatementDescription(..), ExpandParams,
                                             StripeList (..), TokenId (..))
import           Web.Stripe.Types.Util      (getChargeId)

------------------------------------------------------------------------------
-- | Create a `Charge`
data CreateCharge
type instance StripeReturn CreateCharge = Charge
instance StripeHasParam CreateCharge CustomerId
instance StripeHasParam CreateCharge NewCard
instance StripeHasParam CreateCharge TokenId
instance StripeHasParam CreateCharge Description
instance StripeHasParam CreateCharge MetaData
instance StripeHasParam CreateCharge StatementDescription
instance StripeHasParam CreateCharge ReceiptEmail
instance StripeHasParam CreateCharge ApplicationFeeAmount

createCharge
    :: Amount
    -> Currency
    -> StripeRequest CreateCharge
createCharge
    amount
    currency = request
  where request = mkStripeRequest POST url params
        url     = "charges"
        params  = toStripeParam amount   $
                  toStripeParam currency $
                  []

------------------------------------------------------------------------------
-- | Retrieve a `Charge` by `ChargeId`
data GetCharge
type instance StripeReturn GetCharge = Charge
getCharge
    :: ChargeId -- ^ The `Charge` to retrive
    -> StripeRequest GetCharge
getCharge chargeid = getChargeExpandable chargeid []

------------------------------------------------------------------------------
-- | Retrieve a `Charge` by `ChargeId` with `ExpandParams`
getChargeExpandable
    :: ChargeId     -- ^ The `Charge` retrive
    -> ExpandParams -- ^ The `ExpandParams` to retrive
    -> StripeRequest GetCharge
getChargeExpandable
    chargeid
    expandParams = request
  where request = mkStripeRequest GET url params
        url     = "charges" </> getChargeId chargeid
        params  = toExpandable expandParams

------------------------------------------------------------------------------
-- | A `Charge` to be updated
data UpdateCharge
type instance StripeReturn UpdateCharge = Charge
instance StripeHasParam UpdateCharge Description
instance StripeHasParam UpdateCharge MetaData
updateCharge
    :: ChargeId    -- ^ The `Charge` to update
    -> StripeRequest UpdateCharge
updateCharge
    chargeid    = request
  where request = mkStripeRequest POST url params
        url     = "charges" </> getChargeId chargeid
        params  = []

------------------------------------------------------------------------------
-- | a `Charge` to be captured
data CaptureCharge
type instance StripeReturn CaptureCharge = Charge
instance StripeHasParam CaptureCharge Amount
instance StripeHasParam CaptureCharge ReceiptEmail
captureCharge
    :: ChargeId     -- ^ The `ChargeId` of the `Charge` to capture
    -> StripeRequest CaptureCharge
captureCharge
    chargeid     = request
  where request  = mkStripeRequest POST url params
        url      = "charges" </> getChargeId chargeid </> "capture"
        params   = []

------------------------------------------------------------------------------
-- | Retrieve all `Charge`s
data GetCharges
type instance StripeReturn GetCharges = StripeList Charge
instance StripeHasParam GetCharges Created
instance StripeHasParam GetCharges CustomerId
instance StripeHasParam GetCharges (EndingBefore ChargeId)
instance StripeHasParam GetCharges Limit
instance StripeHasParam GetCharges (StartingAfter ChargeId)
getCharges
    :: StripeRequest GetCharges
getCharges =
      getChargesExpandable []

------------------------------------------------------------------------------
-- | Retrieve all `Charge`s
getChargesExpandable
    :: ExpandParams             -- ^ Get Charges with `ExpandParams`
    -> StripeRequest GetCharges
getChargesExpandable
    expandParams = request
  where request = mkStripeRequest GET url params
        url     = "charges"
        params  = toExpandable expandParams
