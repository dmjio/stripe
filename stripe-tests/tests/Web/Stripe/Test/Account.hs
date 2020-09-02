{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE RankNTypes #-}
module Web.Stripe.Test.Account where

import           Data.Either
import           Test.Hspec
import           Web.Stripe.Test.Prelude
import           Web.Stripe.Account

accountTests :: StripeSpec
accountTests stripe = do
  describe "Account tests" $ do
    it "Succesfully retrieves account information" $ do
      result <- stripe $ do d <- getAccountDetails
                            return d
      result `shouldSatisfy` isRight
