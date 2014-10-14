module Web.Stripe.Client
    ( -- * Run a `Stripe` action
      runStripe
      -- * Base type for `Stripe` API
    , Stripe
      -- * Encapsulates `Stripe` secret key and API version information
    , StripeConfig       (..)
    ) where

import Web.Stripe.Client.Internal


