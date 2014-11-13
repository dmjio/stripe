{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RebindableSyntax #-}
module Test.Recipient where

import           Data.Either
import           Test.Hspec
import           Test.Prelude
import           Web.Stripe.Recipient

recipientTests :: StripeSpec
recipientTests stripe = do
  describe "Recipient tests" $ do
    it "Succesfully creates an Individual Recipient" $ do
      result <- stripe $ do
        recipient@Recipient { recipientId = rid } <- createRecipient
          firstName
          lastName
          initial
          Individual
        void $ deleteRecipient rid
        return recipient
      result `shouldSatisfy` isRight
      let Right Recipient {..} = result
      recipientType `shouldBe` Individual
    it "Succesfully creates a Corporation Recipient" $ do
      result <- stripe $ do
        recipient@Recipient { recipientId = rid } <-
           createRecipient
             firstName
             lastName
             initial
             Corporation
        void $ deleteRecipient rid
        return recipient
      result `shouldSatisfy` isRight
      let Right Recipient {..} = result
      recipientType `shouldBe` Corporation
    it "Succesfully retrieves a Recipient" $ do
      result <- stripe $ do
        Recipient { recipientId = rid } <-
          createRecipient
            firstName
            lastName
            initial
            Corporation
        recipient <- getRecipient rid
        void $ deleteRecipient rid
        return recipient
      result `shouldSatisfy` isRight
      let Right Recipient {..} = result
      recipientType `shouldBe` Corporation
    it "Succesfully retrieves a Recipient" $ do
      result <- stripe $ do
        Recipient { recipientId = rid } <-
          createRecipient
            firstName
            lastName
            initial
            Corporation
        recipient <- getRecipient rid
        void $ deleteRecipient rid
        return recipient
      result `shouldSatisfy` isRight
      let Right Recipient {..} = result
      recipientType `shouldBe` Corporation
    it "Succesfully updates a Recipient" $ do
      result <- stripe $ do
        Recipient { recipientId = rid } <-
          createRecipient
            firstName
            lastName
            initial
            Corporation
        recipient <- updateRecipientBase rid
                     (Just firstName)
                     (Just lastName)
                     initial
                     taxid
                     country
                     routingnumber
                     accountnumber
                     Nothing Nothing Nothing Nothing Nothing
                     Nothing email description meta
        void $ deleteRecipient rid
        return recipient
      result `shouldSatisfy` isRight
      let Right Recipient {..} = result
      recipientType `shouldBe` Corporation
      recipientName `shouldBe` "david M johnson"
      recipientDescription `shouldBe` description
      recipientEmail `shouldBe` email
    it "Succesfully deletes a Recipient" $ do
      result <- stripe $ do
        Recipient { recipientId = rid } <-
          createRecipient
            firstName
            lastName
            initial
            Corporation
        void $ deleteRecipient rid
      result `shouldSatisfy` isRight


  where firstName = FirstName "david"
        lastName  = LastName "johnson"
        initial   = Just 'M'
        meta      = [("this", "thing")]
        email     = Just $ Email "djohnson.m@gmail.com"
        country   = Just $ Country "US"
        description = Just "description"
        routingnumber = Just $ RoutingNumber "110000000"
        accountnumber = Just $ AccountNumber "000123456789"
        taxid = Just "000000000"
