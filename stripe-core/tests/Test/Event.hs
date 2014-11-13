{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
module Test.Event where

import           Data.Either
import           Test.Hspec
import           Test.Prelude
import           Web.Stripe.Event

eventTests :: StripeSpec
eventTests stripe = do
  describe "Event tests" $ do
    it "Succesfully retrieves events" $ do
      result <- stripe $ void $ getEvents Nothing Nothing Nothing
      result `shouldSatisfy` isRight


