{-# LANGUAGE OverloadedStrings #-}
module Test.Event where

import           Data.Either

import           Test.Config        (getConfig)
import           Test.Hspec

import           Web.Stripe
import           Web.Stripe.Event

eventTests :: Spec
eventTests = do
  describe "Event tests" $ do
    it "Succesfully retrieves events" $ do
      config <- getConfig
      result <- stripe config $ getEvents Nothing Nothing Nothing
      print result
      result `shouldSatisfy` isRight


