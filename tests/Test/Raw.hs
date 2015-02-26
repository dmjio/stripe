{-# LANGUAGE OverloadedStrings #-}
module Test.Raw (rawTest) where

import           Data.Aeson
import           Data.Either
import           Test.Hspec
import           Web.Stripe
import           Network.Http.Client

rawTest :: StripeConfig -> Spec
rawTest config = do
  describe "Raw tests" $ do
    it "Succesfully retrieves account information" $ do
      result <- (stripeRaw config req) :: IO (Either StripeError Value)
      result `shouldSatisfy` isRight
  where
    req :: StripeRequest 
    req = StripeRequest GET "events" []

                   
