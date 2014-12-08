{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RebindableSyntax #-}
module Web.Stripe.Test.Recipient where

import           Data.Either
import           Test.Hspec
import           Web.Stripe.Test.Prelude
import           Web.Stripe.Recipient

recipientTests :: StripeSpec
recipientTests stripe = do
  describe "Recipient tests" $ do
    it "Succesfully creates an Individual Recipient" $ do
      result <- stripe $ do
        recipient@Recipient { recipientId = rid } <- createRecipient
          name
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
             name
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
            name
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
            name
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
            name
            Corporation
        recipient <- updateRecipient rid
                       -&- (Name "David R. Johnson")
                       -&- taxid
                       -&- (NewBankAccount country routingnumber accountnumber)
                       -&- email
                       -&- description
                       -&- meta
        void $ deleteRecipient rid
        return recipient
      result `shouldSatisfy` isRight
      let Right Recipient {..} = result
      recipientType `shouldBe` Corporation
      recipientName `shouldBe` (Name "David R. Johnson")
      recipientDescription `shouldBe` (Just description)
      recipientEmail `shouldBe` (Just email)
    it "Succesfully deletes a Recipient" $ do
      result <- stripe $ do
        Recipient { recipientId = rid } <-
          createRecipient
            name
            Corporation
        void $ deleteRecipient rid
      result `shouldSatisfy` isRight
  where name      = Name "David M. Johnson"
        meta      = MetaData [("this", "thing")]
        email     = Email "djohnson.m@gmail.com"
        country   = Country "US"
        description = Description "description"
        routingnumber = RoutingNumber "110000000"
        accountnumber = AccountNumber "000123456789"
        taxid = TaxID "000000000"
