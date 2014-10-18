{-# LANGUAGE OverloadedStrings #-}
module Test.Plan where

import           Control.Applicative
import           Data.Either
import qualified Data.Text           as T
import           System.Random
import           Test.Config         (getConfig)
import           Test.Hspec
import           Control.Monad
import           Data.Text           (Text)

import           Web.Stripe
import           Web.Stripe.Plan

makePlanId :: IO Text
makePlanId = T.pack <$> replicateM 10 (randomRIO ('a', 'z'))

planTests :: Spec
planTests = do
  describe "Plan tests" $ do
    it "Succesfully creates a Plan" $ do
      config <- getConfig
      planid <- makePlanId
      result <- stripe config $ do
        p <- createPlan (PlanId planid)
                        0 -- free plan
                        (Currency "usd")
                        Month
                        "sample plan"
                        []
        void $ deletePlan (PlanId planid)
        return p
      result `shouldSatisfy` isRight
    it "Succesfully deletes a Plan" $ do
      config <- getConfig
      planid <- makePlanId
      result <- stripe config $ do
        Plan { planId = pid } <-
          createPlan (PlanId planid)
          0 -- free plan
          (Currency "usd")
          Month
          "sample plan"
          []
        deletePlan pid
      result `shouldSatisfy` isRight
    it "Succesfully updates a Plan" $ do
      config <- getConfig
      planid <- makePlanId
      result <- stripe config $ do
        Plan { planId = pid } <-
          createPlan (PlanId planid)
          0 -- free plan
          (Currency "usd")
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
      config <- getConfig
      planid <- makePlanId
      result <- stripe config $ do
        Plan { planId = pid } <-
          createPlan (PlanId planid)
          0 -- free plan
          (Currency "usd")
          Month
          "sample plan"
          []
        r <- getPlan pid
        void $ deletePlan pid
        return r
      result `shouldSatisfy` isRight
    it "Succesfully retrieves a list of Plans" $ do
      config <- getConfig
      result <- stripe config $ getPlans Nothing Nothing Nothing
      result `shouldSatisfy` isRight




