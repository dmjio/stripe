{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Test.Config where

import qualified Data.ByteString.Char8 as B8
import           Control.Applicative ((<$>))
import           Web.Stripe
import           Web.Stripe.Balance
import           System.Exit
import           System.Environment

getConfig :: IO (Maybe StripeConfig)
getConfig = foundKey =<< lookupEnv "STRIPEKEY"
  where
    foundKey Nothing = return Nothing
    foundKey (Just str) = do
      let config = StripeConfig (B8.pack str)
      result <- stripe config getBalance
      case result of
       Left err -> print err >> exitFailure
       Right Balance{..} ->
         case balanceLiveMode of
           False -> return (Just config)
           True  -> do putStrLn "You entered your production credentials, woops :)" 
                       exitFailure


