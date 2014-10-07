{-# LANGUAGE OverloadedStrings #-}
module Web.Stripe.Transfer
    ( -- * API 
      createTransfer
    , getTransfer
    , getTransfers
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
                                             getParams, toText, (</>), toMetaData)
import           Web.Stripe.Types           (Amount, Currency, Currency (..),
                                             Limit, RecipientId (..),
                                             StripeList (..), Transfer (..), StartingAfter, EndingBefore,
                                             TransferId (..), MetaData)

------------------------------------------------------------------------------
-- | Create a `Transfer`
createTransfer
    :: RecipientId
    -> Amount
    -> Currency
    -> MetaData
    -> Stripe Transfer
createTransfer
    (RecipientId recipientId)
    amount
    (Currency currency)
    metadata    = callAPI request
  where request = StripeRequest POST url params
        url     = "transfers"
        params  = toMetaData metadata ++ getParams [
                   ("amount", toText `fmap` Just amount)
                 , ("currency",  Just currency)
                 , ("recipient", Just recipientId)
                 ]

------------------------------------------------------------------------------
-- | Retrieve a `Transfer`
getTransfer
    :: TransferId -- ^ `TransferId` associated with the `Transfer` to retrieve
    -> Stripe Transfer
getTransfer (TransferId transferId) = callAPI request
  where request = StripeRequest GET url params
        url     = "transfers" </> transferId
        params  = []

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
    endingBefore = callAPI request
  where request = StripeRequest GET url params
        url     = "transfers"
        params  = getParams [
            ("limit", toText `fmap` limit )
          , ("starting_after", (\(TransferId x) -> x) `fmap` startingAfter)
          , ("ending_before", (\(TransferId x) -> x) `fmap` endingBefore)
          ]

------------------------------------------------------------------------------
-- | Update a `Transfer`
updateTransfer
    :: TransferId
    -> MetaData
    -> Stripe Transfer
updateTransfer
    (TransferId transferId)
    metadata    = callAPI request
  where request = StripeRequest POST url params
        url     = "transfers" </> transferId
        params  = toMetaData metadata

------------------------------------------------------------------------------
-- | Cancel a `Transfer`
cancelTransfer
    :: TransferId
    -> Stripe Transfer
cancelTransfer (TransferId transferId) = callAPI request
  where request = StripeRequest POST url params
        url     = "transfers" </> transferId </> "cancel"
        params  = []
