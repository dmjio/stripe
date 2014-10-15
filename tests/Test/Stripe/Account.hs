{-# LANGUAGE RecordWildCards #-}

module Test.Stripe.Account
    ( runAccountTests
    ) where

import Web.Stripe
import Web.Stripe.Account

import Control.Monad.Trans.Reader (ask)
import Control.Monad.IO.Class (liftIO)

runAccountTests :: StripeTest ()
runAccountTests = do
  config <- ask
  liftIO $ either handleNoAccount handleAccount
      =<< runStripe config getAccountDetails
  where
    handleAccount Account {..} 
        = putStrLn "Retrieved Account Details"
    handleNoAccount
        = error "Couldn't retrieve account details!"
    
  


