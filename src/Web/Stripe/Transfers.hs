{-# LANGUAGE OverloadedStrings #-}
module Web.Stripe.Transfers 
    ( -- * Types
      Transfer   (..) 
    , TransferId (..) 
      -- * API Calls
    , createTransfer
    , getTransfer
    , getTransfers
    , updateTransfer
    , cancelTransfer
    ) where

import           Control.Applicative
import           Web.Stripe.Client.Internal
import           Web.Stripe.Types

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
                   ("amount", toText <$> Just amount)
                 , ("currency",  Just currency)
                 , ("recipient", Just recipientId)
                 ]

getTransfer
    :: TransferId
    -> Stripe Transfer
getTransfer (TransferId transferId) = callAPI request
  where request = StripeRequest GET url params
        url     = "transfers" </> transferId
        params  = []

getTransfers
    :: Limit
    -> Stripe Transfer
getTransfers 
    limit = callAPI request
  where request = StripeRequest GET url params
        url     = "transfers"
        params  = getParams [ 
                   ("limit", toText <$> Just limit) 
                  ]

updateTransfer
    :: TransferId
    -> Stripe Transfer
updateTransfer (TransferId transferId) = callAPI request
  where request = StripeRequest POST url params
        url     = "transfers" </> transferId
        params  = []

cancelTransfer
    :: TransferId
    -> Stripe Transfer
cancelTransfer (TransferId transferId) = callAPI request
  where request = StripeRequest POST url params
        url     = "transfers" </> transferId </> "cancel"
        params  = []
