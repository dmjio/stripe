{-# LANGUAGE OverloadedStrings #-}
module Web.Stripe.Account
    ( AccountId(..)
    , Account(..)
    , getAccountDetails
    ) where

import           Control.Applicative ((<$>), (<*>))
import           Data.Aeson
import           Data.Text           (Text)
import           Web.Stripe.Client.Internal
import           Web.Stripe.Types (Account(..))

getAccountDetails :: Stripe Account
getAccountDetails config = callAPI request
  where request = StripeRequest GET "account" params
        params  = []