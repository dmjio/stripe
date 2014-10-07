{-# LANGUAGE OverloadedStrings #-}
module Web.Stripe.Refunds
    ( -- * API
      createRefund
    , getRefund
    , getRefunds
    , updateRefund
      -- * Types
    , Refund     (..)
    , RefundId   (..)
    , ChargeId   (..)
    , StripeList (..)
    ) where

import           Web.Stripe.Client.Internal (Method (GET, POST, DELETE), Stripe,
                                             StripeRequest (..), callAPI, toText, toMetaData,
                                             getParams, (</>))
import           Web.Stripe.Types           (ChargeId (..), EndingBefore, Limit,
                                             Refund (..), RefundId (..), MetaData,
                                             StartingAfter, StripeList (..))

------------------------------------------------------------------------------
-- | `Refund` a `Charge`
createRefund
    :: ChargeId -- ^ 'ChargeId' associated with the 'Charge' to be refunded
    -> MetaData -- ^ `MetaData` associated with a `Refund`
    -> Stripe Refund
createRefund
    (ChargeId chargeId)
    metadata    = callAPI request
  where request = StripeRequest POST url params
        url     = "charges" </> chargeId </> "refunds"
        params  = toMetaData metadata

------------------------------------------------------------------------------
-- | Retrieve a `Refund` by `ChargeId` and `RefundId`
getRefund
    :: ChargeId -- ^ 'ChargeId' associated with the 'Charge' to be retrieved
    -> RefundId -- ^ 'RefundId' associated with the 'Refund' to be retrieved
    -> Stripe Refund
getRefund
    (ChargeId chargeId)
    (RefundId refId) = callAPI request
   where request = StripeRequest GET url params
         url     = "charges" </> chargeId </> "refunds" </> refId
         params  = []

------------------------------------------------------------------------------
-- | Update a `Refund` by `ChargeId` and `RefundId`
updateRefund
    :: ChargeId -- ^ 'ChargeId' associated with the 'Charge' to be updated
    -> RefundId -- ^ 'RefundId' associated with the 'Refund' to be retrieved
    -> MetaData -- ^ `MetaData` associated with a `Refund`
    -> Stripe Refund
updateRefund
   (ChargeId chargeId)
   (RefundId refId) 
   metadata     = callAPI request
  where request = StripeRequest POST url params
        url     = "charges" </> chargeId </> "refunds" </> refId
        params  = toMetaData metadata

------------------------------------------------------------------------------
-- | Retrieve a lot of Refunds by `ChargeId`
getRefunds
    :: ChargeId               -- ^ 'ChargeId' associated with the 'Charge' to be updated
    -> Limit                  -- ^ Defaults to 10 if `Nothing` specified
    -> StartingAfter RefundId -- ^ Paginate starting after the following `RefundId`
    -> EndingBefore RefundId  -- ^ Paginate ending before the following `RefundId`
    -> Stripe (StripeList Refund)
getRefunds
  (ChargeId chargeId)
  limit
  startingAfter
  endingBefore  = callAPI request
  where request = StripeRequest GET url params
        url     = "charges" </> chargeId </> "refunds"
        params  = getParams [
            ("limit", toText `fmap` limit )
          , ("starting_after", (\(RefundId x) -> x) `fmap` startingAfter)
          , ("ending_before", (\(RefundId x) -> x) `fmap` endingBefore)
          ]


