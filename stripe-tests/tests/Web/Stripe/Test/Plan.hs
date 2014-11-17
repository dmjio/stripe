{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
module Web.Stripe.Test.Plan where

import           Data.Either   (Either(..), isRight)

import           Test.Hspec
import           Web.Stripe.Test.Util     (makePlanId)
import           Web.Stripe.Test.Prelude

import           Web.Stripe.Plan

planTests :: StripeSpec
planTests stripe = do
  describe "Plan tests" $ do
    it "Succesfully creates a Plan" $ do
      planid <- makePlanId
      result <- stripe $ do
        p <- createPlan planid
                        0 -- free plan
                        USD
                        Month
                        "sample plan"
                        []
        void $ deletePlan planid
        return p
      result `shouldSatisfy` isRight
    it "Succesfully deletes a Plan" $ do
      planid <- makePlanId
      result <- stripe $ do
        Plan { planId = pid } <-
          createPlan planid
          0 -- free plan
          USD
          Month
          "sample plan"
          []
        void $ deletePlan pid
      result `shouldSatisfy` isRight
    it "Succesfully updates a Plan" $ do
      planid <- makePlanId
      result <- stripe $ do
        Plan { planId = pid } <-
          createPlan planid
          0 -- free plan
          USD
          Month
          "sample plan"
          []
        r <- updatePlanBase pid (Just "cookie")
                                (Just "test")
                                [("key","value")]
        void $ deletePlan pid
        return r
      result `shouldSatisfy` isRight
      let Right Plan { planMetaData = pm
                     , planName = pname
                     , planDescription = pdesc
                     } = result
      pm `shouldBe` [("key", "value")]
      pname `shouldBe` "cookie"
      pdesc `shouldBe` Just "test"

    it "Succesfully retrieves a Plan" $ do
      planid <- makePlanId
      result <- stripe $ do
        Plan { planId = pid } <-
          createPlan planid
          0 -- free plan
          USD
          Month
          "sample plan"
          []
        r <- getPlan pid
        void $ deletePlan pid
        return r
      result `shouldSatisfy` isRight
    it "Succesfully retrieves a list of Plans" $ do
      result <- stripe $ void $ getPlans Nothing Nothing Nothing
      result `shouldSatisfy` isRight




