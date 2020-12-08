{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
module Web.Stripe.Test.Config where

import qualified Data.ByteString.Char8 as B8
import           Control.Applicative ((<$>))

import           Web.Stripe.Client
import           Web.Stripe.Balance
import           System.Exit
import           System.Environment
import           Web.Stripe.Test.Prelude (Stripe, stripeLift)

getConfig :: (forall a. StripeConfig -> Stripe a -> IO (Either StripeError a)) -> IO StripeConfig
getConfig stripe = maybe exitSuccess foundKey =<< lookupEnv "STRIPEKEY"
  where
    foundKey = return . flip StripeConfig Nothing . StripeKey . B8.pack
