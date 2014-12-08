{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
module Web.Stripe.Test.Event where

import           Data.Either
import           Test.Hspec
import           Web.Stripe.Test.Prelude
import           Web.Stripe.Event

eventTests :: StripeSpec
eventTests stripe = do
  describe "Event tests" $ do
    it "Succesfully retrieves events" $ do
      result <- stripe $ void $ getEvents
      result `shouldSatisfy` isRight
