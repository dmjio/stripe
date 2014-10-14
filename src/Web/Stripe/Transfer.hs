{-# LANGUAGE OverloadedStrings #-}
module Web.Stripe.Transfer
    ( -- * API
      createTransfer
    , getTransfer
    , getTransferExpandable
    , getTransfers
    , getTransfersExpandable
    , updateTransfer
    , cancelTransfer
      -- * Types
    , Transfer    (..)
    , TransferId  (..)
    , RecipientId (..)
    , StripeList  (..)
    , Amount
    , Currency
    , Limit
    ) where

import           Web.Stripe.Client.Internal (Method (GET, POST), Stripe,
                                             StripeRequest (..), callAPI,
                                             getParams, toMetaData, toText,
                                             (</>), toExpandable)
import           Web.Stripe.Types           (Amount, Currency, Currency (..),
                                             EndingBefore, Limit, MetaData,
                                             RecipientId (..), StartingAfter,
                                             StripeList (..), Transfer (..),
                                             TransferId (..), ExpandParams)
import           Web.Stripe.Types.Util      (getRecipientId)

------------------------------------------------------------------------------
-- | Create a `Transfer`
createTransfer
    :: RecipientId
    -> Amount
    -> Currency
    -> MetaData
    -> Stripe Transfer
createTransfer
    recipientid
    amount
    (Currency currency)
    metadata    = callAPI request
  where request = StripeRequest POST url params
        url     = "transfers"
        params  = toMetaData metadata ++ getParams [
                   ("amount", toText `fmap` Just amount)
                 , ("currency",  Just currency)
                 , ("recipient", getRecipientId `fmap` Just recipientid)
                 ]

------------------------------------------------------------------------------
-- | Retrieve a `Transfer`
getTransfer
    :: TransferId -- ^ `TransferId` associated with the `Transfer` to retrieve
    -> Stripe Transfer
getTransfer transferid =
    getTransferExpandable transferid []

------------------------------------------------------------------------------
-- | Retrieve a `Transfer` with `ExpandParams`
getTransferExpandable
    :: TransferId -- ^ `TransferId` associated with the `Transfer` to retrieve
    -> ExpandParams
    -> Stripe Transfer
getTransferExpandable
    (TransferId transferid)
    expandParams = callAPI request
  where request = StripeRequest GET url params
        url     = "transfers" </> transferid
        params  = toExpandable expandParams

------------------------------------------------------------------------------
-- | Retrieve StripeList of `Transfers`
getTransfers
    :: Limit                    -- ^ Defaults to 10 if `Nothing` specified
    -> StartingAfter TransferId -- ^ Paginate starting after the following `TransferId`
    -> EndingBefore TransferId  -- ^ Paginate ending before the following `TransferId`
    -> Stripe (StripeList Transfer)
getTransfers
    limit
    startingAfter
    endingBefore =
      getTransfersExpandable limit startingAfter endingBefore []

------------------------------------------------------------------------------
-- | Retrieve StripeList of `Transfers` with `ExpandParams`
getTransfersExpandable
    :: Limit                    -- ^ Defaults to 10 if `Nothing` specified
    -> StartingAfter TransferId -- ^ Paginate starting after the following `TransferId`
    -> EndingBefore TransferId  -- ^ Paginate ending before the following `TransferId`
    -> ExpandParams
    -> Stripe (StripeList Transfer)
getTransfersExpandable
    limit
    startingAfter
    endingBefore
    expandParams = callAPI request
  where request = StripeRequest GET url params
        url     = "transfers"
        params  = getParams [
            ("limit", toText `fmap` limit )
          , ("starting_after", (\(TransferId x) -> x) `fmap` startingAfter)
          , ("ending_before", (\(TransferId x) -> x) `fmap` endingBefore)
          ] ++ toExpandable expandParams

------------------------------------------------------------------------------
-- | Update a `Transfer`
updateTransfer
    :: TransferId
    -> MetaData
    -> Stripe Transfer
updateTransfer
    (TransferId transferid)
    metadata    = callAPI request
  where request = StripeRequest POST url params
        url     = "transfers" </> transferid
        params  = toMetaData metadata

------------------------------------------------------------------------------
-- | Cancel a `Transfer`
cancelTransfer
    :: TransferId
    -> Stripe Transfer
cancelTransfer (TransferId transferid) = callAPI request
  where request = StripeRequest POST url params
        url     = "transfers" </> transferid </> "cancel"
        params  = []
