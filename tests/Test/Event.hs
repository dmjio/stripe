{-# LANGUAGE OverloadedStrings #-}
module Test.Event where

import           Data.Either
import           Test.Config        (getConfig)
import           Test.Hspec
import           Web.Stripe
import           Web.Stripe.Account

eventTests :: Spec
eventTests = do
  describe "Event tests" $ do
    it "Succesfully retrieves account information" $ do
      config <- getConfig
      result <- stripe config getAccountDetails
      result `shouldSatisfy` isRight

