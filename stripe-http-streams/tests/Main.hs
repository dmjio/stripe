module Main where

import           Control.Monad.Trans.Free       (FreeF(..), FreeT(..))
import           Network.Http.Client            (Connection)
import           Web.Stripe.Client              (StripeConfig(..), StripeError(..))
import           Web.Stripe.Client.HttpStreams  (withConnection, callAPI)
import           Web.Stripe.StripeRequest       (StripeRequest(..))
import           Web.Stripe.Test.AllTests       (allTests)
import           Web.Stripe.Test.Prelude        (Stripe, StripeRequestF(..))

main :: IO ()
main = allTests runStripe

runStripe :: StripeConfig
          -> Stripe a
          -> IO (Either StripeError a)
runStripe config stripe =
  withConnection $ \conn ->
    runStripe' conn config stripe

runStripe' :: Connection
           -> StripeConfig
           -> Stripe a
           -> IO (Either StripeError a)
runStripe' conn config (FreeT m) =
  do f <- m
     case f of
       (Pure a) -> return (Right a)
       (Free (StripeRequestF req decode)) ->
         do r <- callAPI conn decode config req
            case r of
              (Left e) ->  return (Left e)
              (Right next) -> runStripe' conn config next






