{-# LANGUAGE CPP #-}
module Main where

import Control.Monad.Trans.Free       (FreeF(..), FreeT(..))
#if MIN_VERSION_http_client(0,5,13)
import Network.HTTP.Client hiding (withConnection)
#else
import Network.HTTP.Client
#endif
import Web.Stripe.Client              (StripeConfig(..), StripeError(..))
import Web.Stripe.Client.HttpClient   (withConnection, callAPI)
import Web.Stripe.Test.AllTests       (allTests)
import Web.Stripe.Test.Prelude        (Stripe, StripeRequestF(..))

main :: IO ()
main = allTests runStripe

runStripe :: StripeConfig
          -> Stripe a
          -> IO (Either StripeError a)
runStripe config stripe =
  withConnection $ \conn ->
    runStripe' conn config stripe

runStripe' :: Manager
           -> StripeConfig
           -> Stripe a
           -> IO (Either StripeError a)
runStripe' manager config (FreeT m) =
  do f <- m
     case f of
       (Pure a) -> return (Right a)
       (Free (StripeRequestF req decode')) ->
         do r <- callAPI manager decode' config req
            case r of
              (Left e) ->  return (Left e)
              (Right next) -> runStripe' manager config next
