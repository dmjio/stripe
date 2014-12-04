{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
-------------------------------------------
-- |
-- Module      : Web.Stripe.Dispute
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- < https:/\/\stripe.com/docs/api#diputes >
--
-- @
-- import Web.Stripe
-- import Web.Stripe.Charge
-- import Web.Stripe.Dispute
--
-- main :: IO ()
-- main = do
--   let config = SecretKey "secret_key"
--   result <- stripe config $ do
--     Charge { chargeDispute = dispute } <- getCharge (ChargeId "charge_id")
--     return dispute
--   case result of
--     Right (Just dispute) -> print dispute
--     Right Nothing        -> print "no dispute on this charge"
--     Left  stripeError    -> print stripeError
-- @
module Web.Stripe.Dispute
    ( -- * API
      UpdateDispute
    , updateDispute
    , CloseDispute
    , closeDispute
      -- * Types
    , ChargeId      (..)
    , Dispute       (..)
    , DisputeReason (..)
    , DisputeStatus (..)
    , Evidence      (..)
    , MetaData      (..)
    ) where

import           Web.Stripe.StripeRequest (Method (POST),
                                           StripeHasParam, StripeRequest (..),
                                           StripeReturn,
                                           mkStripeRequest)
import           Web.Stripe.Util          ((</>))
import           Web.Stripe.Types         (ChargeId (..), Dispute (..),
                                           DisputeReason (..),
                                           DisputeStatus (..),
                                           Evidence (..), MetaData(..))
import           Web.Stripe.Types.Util    (getChargeId)

------------------------------------------------------------------------------
-- | `Dispute` to be updated
data UpdateDispute
type instance StripeReturn UpdateDispute = Dispute
instance StripeHasParam UpdateDispute Evidence
instance StripeHasParam UpdateDispute MetaData
updateDispute
    :: ChargeId        -- ^ The ID of the Charge being disputed
    -> StripeRequest UpdateDispute
updateDispute
  chargeId = request
  where request = mkStripeRequest POST url params
        url     = "charges" </> getChargeId chargeId </> "dispute"
        params  = []

------------------------------------------------------------------------------
-- | `Dispute` to be closed
data CloseDispute
type instance StripeReturn CloseDispute = Dispute
closeDispute
    :: ChargeId  -- ^ The ID of the Charge being disputed
    -> StripeRequest CloseDispute
closeDispute
    chargeId = request
  where request = mkStripeRequest POST url params
        url     = "charges" </> getChargeId chargeId </> "dispute" </> "close"
        params  = []
