{-# LANGUAGE OverloadedStrings #-}

module Web.Stripe.Transfers where

import qualified Data.Text.Encoding         as T
import           Web.Stripe.Client.Internal
import           Web.Stripe.Types

createTransfer :: RecipientId -> Stripe Transfer
createTransfer (RecipientId recipientId) = callAPI request
  where request = StripeRequest POST url params
        url     = "transfers"
        params  = [ ("amount", "400")
                  , ("currency", "usd")
                  , ("recipient", T.encodeUtf8 recipientId)
                  ]

getTransfer :: TransferId -> Stripe Transfer
getTransfer (TransferId transferId) = callAPI request
  where request = StripeRequest GET url params
        url     = "transfers" </> transferId
        params  = []

updateTransfer :: TransferId -> Stripe Transfer
updateTransfer (TransferId transferId) = callAPI request
  where request = StripeRequest POST url params
        url     = "transfers" </> transferId
        params  = []

cancelTransfer :: TransferId -> Stripe Transfer
cancelTransfer (TransferId transferId) = callAPI request
  where request = StripeRequest POST url params
        url     = "transfers" </> transferId </> "cancel"
        params  = []
