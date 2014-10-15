module Web.Stripe.Client
    ( -- * Run a `Stripe` action
      stripe
      -- * Base type for `Stripe` API
    , Stripe
      -- * Encapsulates `Stripe` Secret Key 
    , StripeConfig       (..)
      -- * Error type for Request
    , module Web.Stripe.Client.Error
    ) where

import Web.Stripe.Client.Internal
import Web.Stripe.Client.Error


