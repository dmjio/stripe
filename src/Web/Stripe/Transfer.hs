{-# LANGUAGE OverloadedStrings #-}
-------------------------------------------
-- |
-- Module      : Web.Stripe.Transfer
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- < https:/\/\stripe.com/docs/api#transfers >
--
-- @
-- import Web.Stripe         
-- import Web.Stripe.Transfer
-- import Web.Stripe.Recipient
--
-- main :: IO ()
-- main = do
--   let config = SecretKey "secret_key"
--   result <- stripe config $ do
--     Recipient { recipientId = recipientid } <- getRecipient (RecipientId "recipient_id")
--     createTransfer recipientid (100 :: Amount) USD ([] :: MetaData)
--   case result of
--     Right transfer    -> print transfer
--     Left  stripeError -> print stripeError
-- @
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
    , Transfer        (..)
    , TransferId      (..)
    , TransferStatus  (..)
    , TransferType    (..)
    , RecipientId     (..)
    , Recipient       (..)
    , StripeList      (..)
    , Currency        (..)
    , Amount
    , Limit
    ) where

import           Web.Stripe.Client.Internal (Method (GET, POST), Stripe,
                                             StripeRequest (..), callAPI,
                                             getParams, toExpandable, toTextLower,
                                             toMetaData, toText, (</>))
import           Web.Stripe.Types           (Amount, Currency (..),
                                             EndingBefore, ExpandParams, Limit,
                                             MetaData, RecipientId (..), Recipient(..),
                                             StartingAfter, StripeList (..),
                                             Transfer (..), TransferId (..),
                                             TransferStatus (..),Description,
                                             TransferType (..))
import           Web.Stripe.Types.Util      (getRecipientId)

------------------------------------------------------------------------------
-- | Create a `Transfer`
createTransfer
    :: RecipientId -- ^ The `RecipientId` of the `Recipient` who will receive the `Transfer`
    -> Amount      -- ^ The `Amount` of money to transfer to the `Recipient`
    -> Currency    -- ^ The `Currency` in which to perform the `Transfer`
    -> MetaData    -- ^ The `MetaData` associated with the Transfer
    -> Stripe Transfer
createTransfer
    recipientid
    amount
    currency
    metadata    = callAPI request
  where request = StripeRequest POST url params
        url     = "transfers"
        params  = toMetaData metadata ++ getParams [
                   ("amount", toText `fmap` Just amount)
                 , ("currency",  toTextLower `fmap` Just currency)
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
    :: TransferId   -- ^ `TransferId` associated with the `Transfer` to retrieve
    -> ExpandParams -- ^ The `ExpandParams` of the object to be expanded
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
    -> ExpandParams             -- ^ The `ExpandParams` of the object to be expanded
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
    :: TransferId        -- ^ The `TransferId` of the `Transfer` to update
    -> Maybe Description -- ^ The `Description` of the `Transfer` to update
    -> MetaData          -- ^ The `MetaData` of the `Transfer` to update
    -> Stripe Transfer
updateTransfer
    (TransferId transferid)
    description
    metadata    = callAPI request
  where request = StripeRequest POST url params
        url     = "transfers" </> transferid
        params  = toMetaData metadata ++ getParams [
           ("description", description)
          ]  

------------------------------------------------------------------------------
-- | Cancel a `Transfer`
cancelTransfer
    :: TransferId        -- ^ The `TransferId` of the `Transfer` to cancel
    -> Stripe Transfer
cancelTransfer (TransferId transferid) = callAPI request
  where request = StripeRequest POST url params
        url     = "transfers" </> transferid </> "cancel"
        params  = []
