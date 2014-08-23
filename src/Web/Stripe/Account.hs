{-# LANGUAGE OverloadedStrings #-}
module Web.Stripe.Account
    ( AccountId(..)
    , Account(..)
    , getAccountDetails
    ) where

import           Control.Applicative        ((<$>), (<*>))
import           Data.Text                  (Text)
import           Web.Stripe.Client.Internal (Method (GET), Stripe,
                                             StripeRequest (StripeRequest),
                                             callAPI)
import           Web.Stripe.Types           (Account (..), AccountId (..))

-- | Retrieve the object that represents your Stripe account
getAccountDetails :: Stripe Account
getAccountDetails = callAPI request
  where request = StripeRequest GET url params
        url     = "account"
        params  = []
