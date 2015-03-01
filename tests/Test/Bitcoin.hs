{-# LANGUAGE OverloadedStrings #-}
module Test.Bitcoin ( bitcoinTests ) where

import           Data.Either
import           Test.Hspec
import           Web.Stripe
import           Web.Stripe.Bitcoin

bitcoinTests :: StripeConfig -> Spec
bitcoinTests config = do
  describe "Bitcoin tests" $ do
    it "Succesfully creates a bitcoin receiver" $ do
      result <- stripe config $ createReceiver 10 (Email "fake@gmail.com")
      result `shouldSatisfy` isRight
    it "Succesfully retrieves a bitcoin receiver" $ do
      result <- stripe config $ getReceiver
                 (BitcoinReceiverId "btcrcv_15ahwMDKTaiM5evRNmR1d2Xk")
      result `shouldSatisfy` isRight
    it "Succesfully lists bitcoin receivers" $ do
      result <- stripe config $ listReceivers Nothing Nothing Nothing
      result `shouldSatisfy` isRight
