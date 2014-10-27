{-# LANGUAGE OverloadedStrings #-}
module Test.Config where

import           Data.ByteString (ByteString)
import           Web.Stripe

getConfig :: IO StripeConfig
getConfig = return $ StripeConfig key
  where
    key :: ByteString
    key = "sk_test_igoYowTqR5IfovOKFKwigRmW"
