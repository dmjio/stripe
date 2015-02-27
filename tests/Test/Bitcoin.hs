{-# LANGUAGE OverloadedStrings #-}
module Test.Bitcoin ( bitcoinTests ) where

import           Data.Either
import           Test.Hspec
import           Web.Stripe
import           Web.Stripe.Bitcoin
import           Web.Stripe.Types

bitcoinTests :: StripeConfig -> Spec
bitcoinTests config = do
  describe "Bitcoin tests" $ do
    it "Succesfully creates a bitcoin receiver" $ do
--      result <- stripe config $ createReceiver undefined undefined
      result2 <- stripe config listReceivers
      print result2
      result3 <- stripe config $ getReceiver (BitcoinReceiverId "btcrcv_15ahwMDKTaiM5evRNmR1d2Xk")

--      print result
--      print result2
      print result3
      result3 `shouldSatisfy` isRight

