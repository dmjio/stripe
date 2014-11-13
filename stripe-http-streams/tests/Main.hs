module Main where

import           AllTests
import           Control.Monad.Trans.Free (FreeF(..), FreeT(..))
import           Network.Http.Client        (Connection)
import           Test.Prelude (StripeF(..), Stripe)
import           Web.Stripe.Client
import           Web.Stripe.Client.HttpStreams

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
       (Free (StripeF decode req)) ->
         do r <- callAPI conn decode config req
            case r of
              (Left e) ->  return (Left e)
              (Right next) -> runStripe' conn config next






