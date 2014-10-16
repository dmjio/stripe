module Test.Config where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Web.Stripe

getConfig :: IO StripeConfig
getConfig = return $ StripeConfig key
  where
    key :: ByteString
    key = "sk_test_zvqdM2SSA6WwySqM6KJQrqpH"
