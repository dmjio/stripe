{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Transfer where

import           Data.Either
import           Control.Monad
import           Test.Hspec

import           Web.Stripe
import           Web.Stripe.Recipient
import           Web.Stripe.Transfer

transferTests :: StripeConfig -> Spec
transferTests config = do
  describe "Transfer tests" $ do
    it "Create a new transfer" $ do
      result <- stripe config $ do
        Recipient { recipientId = rid } <-
          createRecipientByBank
            firstname
            lastname
            Nothing
            Individual
            country
            routingnumber
            accountnumber
        transfer <- createTransfer rid (100 :: Amount) USD []
        void $ deleteRecipient rid
        return transfer
      result `shouldSatisfy` isRight
    it "Retrieves a transfer" $ do
      result <- stripe config $ do
        Recipient { recipientId = rid } <-
          createRecipientByBank
            firstname
            lastname
            Nothing
            Individual
            country
            routingnumber
            accountnumber
        Transfer { transferId = tid }
           <- createTransfer rid (100 :: Amount) USD []
        t <- getTransfer tid
        void $ deleteRecipient rid
        return t
      result `shouldSatisfy` isRight
    it "Retrieves a transfer expandable" $ do
      result <- stripe config $ do
        Recipient { recipientId = rid } <-
          createRecipientByBank
            firstname
            lastname
            Nothing
            Individual
            country
            routingnumber
            accountnumber
        Transfer { transferId = tid }
           <- createTransfer rid (100 :: Amount) USD []
        t <- getTransferExpandable tid ["recipient", "balance_transaction"]
        void $ deleteRecipient rid
        return t
      result `shouldSatisfy` isRight
    it "Retrieves transfers" $ do
      result <- stripe config $ getTransfers Nothing Nothing Nothing 
      result `shouldSatisfy` isRight
    it "Retrieves transfers expandable" $ do
      result <- stripe config $ getTransfersExpandable Nothing Nothing Nothing
                  [ "data.recipient"
                  , "data.balance_transaction"
                  ]
      result `shouldSatisfy` isRight
    it "Updates a transfer" $ do
      result <- stripe config $ do
        Recipient { recipientId = rid } <-
          createRecipientByBank
            firstname
            lastname
            Nothing
            Individual
            country
            routingnumber
            accountnumber
        Transfer { transferId = tid }
           <- createTransfer rid (100 :: Amount) USD []
        t <- updateTransfer tid (Just "hey there") [("hey", "there")]
        void $ deleteRecipient rid
        return t
      result `shouldSatisfy` isRight
      let Right Transfer {..} = result
      transferMetaData `shouldBe` [("hey", "there")]   
      transferDescription `shouldBe` Just "hey there"
    it "Can't Cancel a committed transfer" $ do
      result <- stripe config $ do
        Recipient { recipientId = rid } <-
          createRecipientByBank
            firstname
            lastname
            Nothing
            Individual
            country
            routingnumber
            accountnumber
        Transfer { transferId = tid }
           <- createTransfer rid (100 :: Amount) USD []
        t <- cancelTransfer tid 
        void $ deleteRecipient rid
        return t
      result `shouldSatisfy` isLeft
  where
    country       = Country "US"
    routingnumber = RoutingNumber "110000000"
    accountnumber = AccountNumber "000123456789"
    firstname     = FirstName "David"
    lastname      = LastName "Johnson"

