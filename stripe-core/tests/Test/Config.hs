{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Test.Config where

import qualified Data.ByteString.Char8 as B8
import           Control.Applicative ((<$>))

import           Web.Stripe
import           Web.Stripe.Balance
import           System.Exit
import           System.Environment

getConfig :: IO StripeConfig
getConfig = maybe enterKey foundKey =<< lookupEnv "STRIPEKEY"
  where
    foundKey = return . StripeConfig . B8.pack
    enterKey = do
      putStrLn "Please enter your Stripe *TEST* account"
      config <- StripeConfig . B8.pack <$> getLine
      result <- stripe config getBalance
      case result of
       Left err -> print err >> exitFailure
       Right Balance{..} ->
         case balanceLiveMode of
         False -> return config
         True  -> do putStrLn "You entered your production credentials, woops :)" 
                     exitFailure


