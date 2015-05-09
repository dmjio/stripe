{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Bitcoin ( bitcoinTests ) where

import           Data.Either
import           Test.Hspec
import           Web.Stripe
import           Web.Stripe.Bitcoin

bitcoinTests :: StripeConfig -> Spec
bitcoinTests config = do
  describe "Bitcoin tests" $ do
    result <- runIO $ stripe config $ createReceiver 10 (Email "fake@gmail.com")
    it "Succesfully creates a bitcoin receiver" $ do
      result `shouldSatisfy` isRight
    it "Succesfully retrieves a bitcoin receiver" $ do
      let Right BitcoinReceiver {..} = result
      result <- stripe config $ getReceiver btcId
      result `shouldSatisfy` isRight
    it "Succesfully lists bitcoin receivers" $ do
      result <- stripe config $ listReceivers Nothing Nothing Nothing
      result `shouldSatisfy` isRight
