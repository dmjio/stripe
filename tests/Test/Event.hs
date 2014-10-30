{-# LANGUAGE OverloadedStrings #-}
module Test.Event where

import           Data.Either

import           Test.Hspec

import           Web.Stripe
import           Web.Stripe.Event

eventTests :: StripeConfig -> Spec
eventTests config = do
  describe "Event tests" $ do
    it "Succesfully retrieves events" $ do
      result <- stripe config $ getEvents Nothing Nothing Nothing
      result `shouldSatisfy` isRight


