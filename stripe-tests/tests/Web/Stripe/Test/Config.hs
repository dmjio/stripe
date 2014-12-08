{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
module Web.Stripe.Test.Config where

import qualified Data.ByteString.Char8 as B8
import           Control.Applicative ((<$>))

import           Web.Stripe
import           Web.Stripe.Balance
import           System.Exit
import           System.Environment
import           Web.Stripe.Test.Prelude (Stripe, stripeLift)

getConfig :: (forall a. StripeConfig -> Stripe a -> IO (Either StripeError a)) -> IO StripeConfig
getConfig stripe = maybe enterKey foundKey =<< lookupEnv "STRIPEKEY"
  where
    foundKey = return . StripeConfig . StripeKey . B8.pack
    enterKey = do
      putStrLn "Please enter your Stripe *TEST* account"
      config <- StripeConfig . StripeKey. B8.pack <$> getLine
      result <- stripe config (stripeLift getBalance)
      case result of
       Left err -> print err >> exitFailure
       Right Balance{..} ->
         case balanceLiveMode of
         False -> return config
         True  -> do putStrLn "You entered your production credentials, woops :)" 
                     exitFailure


