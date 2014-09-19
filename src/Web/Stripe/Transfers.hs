{-# LANGUAGE OverloadedStrings #-}
module Web.Stripe.Transfers
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
                                             getParams, toText, (</>))
import           Web.Stripe.Types           (Amount, Currency, Currency (..),
                                             Limit, RecipientId (..),
                                             StripeList (..), Transfer (..),
                                             TransferId (..))

------------------------------------------------------------------------------
-- | Create a `Transfer`
createTransfer
    :: RecipientId
    -> Amount
    -> Currency
    -> Stripe Transfer
createTransfer
    (RecipientId recipientId)
    amount
    (Currency currency) = callAPI request
  where request = StripeRequest POST url params
        url     = "transfers"
        params  = getParams [
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
    :: Maybe Limit
    -> Stripe (StripeList Transfer)
getTransfers
    limit = callAPI request
  where request = StripeRequest GET url params
        url     = "transfers"
        params  = getParams [
                   ("limit", fmap toText limit)
                  ]

------------------------------------------------------------------------------
-- | Update a `Transfer`
updateTransfer
    :: TransferId
    -> Stripe Transfer
updateTransfer (TransferId transferId) = callAPI request
  where request = StripeRequest POST url params
        url     = "transfers" </> transferId
        params  = []

------------------------------------------------------------------------------
-- | Cancel a `Transfer`
cancelTransfer
    :: TransferId
    -> Stripe Transfer
cancelTransfer (TransferId transferId) = callAPI request
  where request = StripeRequest POST url params
        url     = "transfers" </> transferId </> "cancel"
        params  = []
