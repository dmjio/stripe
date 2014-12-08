{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
module Web.Stripe.Test.Dispute where

import           Data.Either            (Either(Right), isRight)
import           Control.Concurrent     (threadDelay)

import           Test.Hspec
import           Web.Stripe.Test.Prelude
import           Web.Stripe.Test.Util

import           Web.Stripe.Dispute
import           Web.Stripe.Charge
import           Web.Stripe.Customer

disputeTests :: StripeSpec
disputeTests stripe = do
  describe "Dispute Tests" $ do
    it "Creates a Dispute" $ do
      result <- stripe $ do
        Customer { customerId = cid } <- createCustomer -&- cardinfo
        Charge   { chargeId = chid } <- createCharge (Amount 100) USD -&- cid
        liftIO $ threadDelay (secs 20) -- Sleep to allow the thread to dispute to happen
        Charge { chargeDispute = cd } <- getCharge chid
        void $ deleteCustomer cid
        return cd
      result `shouldSatisfy` isRight
      let Right (Just Dispute{..}) = result
      disputeStatus `shouldBe` NeedsResponse

    it "Makes Dispute Under Review" $ do
      result <- stripe $ do
        Customer { customerId = cid } <- createCustomer -&- cardinfo
        Charge   { chargeId = chid  } <- createCharge (Amount 100) USD -&- cid
        liftIO $ threadDelay (secs 10)
        void $ updateDispute chid -&- evi -&- meta
        liftIO $ threadDelay (secs 10)
        Charge { chargeDispute = Just dispute } <- getCharge chid
        void $ deleteCustomer cid
        return dispute
      result `shouldSatisfy` isRight
      let Right Dispute {..} = result
      disputeMetaData `shouldBe` meta
      disputeEvidence `shouldBe` (Just evi)
      disputeStatus `shouldBe` UnderReview
    it "Wins a Dispute" $ do
      result <- stripe $ do
        Customer { customerId = cid } <- createCustomer -&- cardinfo
        Charge   { chargeId = chid  } <- createCharge (Amount 100) USD -&- cid
        liftIO $ threadDelay (secs 10)
        void $ updateDispute chid -&- win -&- meta
        liftIO $ threadDelay (secs 10)
        Charge { chargeDispute = Just dispute } <- getCharge chid
        void $ deleteCustomer cid
        return dispute
      result `shouldSatisfy` isRight
      let Right Dispute {..} = result
      disputeMetaData `shouldBe` meta
      disputeEvidence `shouldBe` (Just win)
      disputeStatus `shouldBe` Won
    it "Loses a Dispute" $ do
      result <- stripe $ do
        Customer { customerId = cid } <- createCustomer -&- cardinfo
        Charge   { chargeId = chid  } <- createCharge (Amount 100) USD -&- cid
        liftIO $ threadDelay (secs 10) -- Sleep to allow the thread to dispute to happen
        void $ updateDispute chid -&- lose -&- meta
        liftIO $ threadDelay (secs 10)
        Charge { chargeDispute = Just dispute } <- getCharge chid
        void $ deleteCustomer cid
        return dispute
      result `shouldSatisfy` isRight
      let Right Dispute {..} = result
      disputeMetaData `shouldBe` meta
      disputeEvidence `shouldBe` (Just lose)
      disputeStatus `shouldBe` Lost
    it "Closes a Dispute" $ do
      result <- stripe $ do
        Customer { customerId = cid } <- createCustomer -&- cardinfo
        Charge   { chargeId = chid  } <- createCharge (Amount 100)  USD -&- cid
        liftIO $ threadDelay (secs 10) -- Sleep to allow the thread to dispute to happen
        dispute <- closeDispute chid
        void $ deleteCustomer cid
        return dispute
      result `shouldSatisfy` isRight
      let Right Dispute {..} = result
      disputeStatus `shouldBe` Lost
  where
    cn  = CardNumber "4000000000000259"
    em  = ExpMonth 12
    ey  = ExpYear 2015
    cvc = CVC "123"
    win  = Evidence "winning_evidence"
    lose = Evidence "losing_evidence"
    evi = Evidence "some evidence"
    meta = MetaData [ ("some", "metadata") ]
    cardinfo =
      (mkNewCard cn em ey) { newCardCVC = Just cvc }
