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
    it "Succesfully retrieves an event" $ do
      config <- getConfig
      result <- stripe config $ getEvent (EventId "evt_50qFQIQZdnt2Sd")
      result `shouldSatisfy` isRight
    it "Succesfully retrieves events" $ do
      config <- getConfig
      result <- stripe config $ getEvents (Just 100) Nothing Nothing
      print result
      result `shouldSatisfy` isRight


