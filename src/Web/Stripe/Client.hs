module Web.Stripe.Client
    ( -- * Run a `Stripe` action
      runStripe
      -- * Base type for `Stripe` API
    , Stripe
      -- * Encapsulates `Stripe` secret key and API version information
    , StripeConfig       (..)
      -- * Error type for Request
    , module Web.Stripe.Client.Error
    ) where

import Web.Stripe.Client.Internal
import Web.Stripe.Client.Error


