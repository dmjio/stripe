module Test.Stripe
    ( StripeTest
    , runTests
    ) where

import           Web.Stripe.Client
import           Control.Monad.Trans.Reader

type StripeTest a = ReaderT StripeConfig IO a

runTests :: Monad m => b -> ReaderT b m a -> m a
runTests = flip runReaderT

