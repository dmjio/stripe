{-# LANGUAGE OverloadedStrings #-}
module Web.Stripe.Recipient 
    ( -- * Types
      Recipient (..)
    , RecipientId (..)
    , RecipientType (..)
    -- * API Calls
    , createRecipient
    , getRecipient
    , updateRecipient
    ) where

import           Control.Applicative
import           Data.Time

import           Web.Stripe.Client.Internal
import           Web.Stripe.Types

createRecipient
    :: Name
    -> RecipientType
    -> Stripe Recipient
createRecipient 
    (Name name)
    recipientType  = callAPI request
  where request = StripeRequest POST url params
        url     = "recipients"
        params  = getParams [ 
                    ("name", Just name)
                  , ("type", toText <$> Just recipientType)
                  ]

getRecipient
    :: RecipientId
    -> Stripe Recipient
getRecipient 
    (RecipientId recipientId) = callAPI request
  where request =  StripeRequest GET url params
        url     = "recipients" </> recipientId
        params  = []

updateRecipient
    :: RecipientId
    -> Stripe Recipient
updateRecipient
    (RecipientId recipientId) = callAPI request
  where request = StripeRequest POST url params
        url     = "recipients" </> recipientId
        params  = []

deleteRecipient
    :: RecipientId
    -> Stripe Recipient
deleteRecipient
   (RecipientId recipientId) = callAPI request
  where request =  StripeRequest DELETE url params
        url     = "recipients" </> recipientId
        params  = []
