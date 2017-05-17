{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
module Web.Stripe.Test.Transfer where

import           Data.Maybe
import           Data.Either
import           Data.String
import           Web.Stripe.Test.Prelude

import           Web.Stripe.Recipient
import           Web.Stripe.Transfer

------------------------------------------------------------------------------
-- the tests

transferTests :: StripeSpec
transferTests stripe =
  describe "Transfer tests" $ do
    -- it "Create a new transfer" $ do
    --   result <- stripe $ do
    --     Recipient { recipientId = rid } <-
    --       createRecipient name Individual -&- bankinfo
    --     transfer <- createTransfer rid (Amount 100) USD
    --     void $ deleteRecipient rid
    --     return transfer
    --   result `shouldSatisfy` isRight
    -- it "Retrieves a transfer" $ do
    --   result <- stripe $ do
    --     Recipient { recipientId = rid } <-
    --       createRecipient name Individual -&- bankinfo
    --     Transfer { transferId = tid }
    --        <- createTransfer rid (Amount 100) USD
    --     t <- getTransfer tid
    --     void $ deleteRecipient rid
    --     return t
    --   result `shouldSatisfy` isRight
    -- it "Retrieves a transfer expandable" $ do
    --   result <- stripe $ do
    --     Recipient { recipientId = rid } <-
    --       createRecipient name Individual -&- bankinfo
    --     Transfer { transferId = tid }
    --        <- createTransfer rid (Amount 100) USD
    --     t <- getTransfer tid -&- ExpandParams ["recipient", "balance_transaction"]
    --     void $ deleteRecipient rid
    --     return t
    --   result `shouldSatisfy` isRight
    it "Retrieves transfers" $ do
      result <- stripe $ do t <- getTransfers
                            return t
      result `shouldSatisfy` isRight
    it "Retrieves transfers expandable" $ do
      result <- stripe $ do t <- getTransfers -&- ExpandParams
                                   [ "data.recipient"
                                   , "data.balance_transaction"
                                   ]
                            return t
      result `shouldSatisfy` isRight
    -- it "Updates a transfer" $ do
    --   result <- stripe $ do
    --     Recipient { recipientId = rid } <-
    --       createRecipient name Individual -&- bankinfo
    --     Transfer { transferId = tid }
    --        <- createTransfer rid (Amount 100) USD
    --     t <- updateTransfer tid
    --           -&- (Description "hey there")
    --           -&- (MetaData [("hey", "there")])
    --     void $ deleteRecipient rid
    --     return t
    --   result `shouldSatisfy` isRight
    --   let Right Transfer {..} = result
    --   transferMetaData `shouldBe` (MetaData [("hey", "there")])
    --   transferDescription `shouldBe` (Just (Description "hey there"))
    it "Can't Cancel a committed transfer" $ do
      result <- stripe $ do
        Recipient { recipientId = rid } <-
          createRecipient
            name
            Individual
            -&- bankinfo
        Transfer { transferId = tid }
           <- createTransfer rid (Amount 100) USD
        t <- cancelTransfer tid
        void $ deleteRecipient rid
        return t
      result `shouldSatisfy` isLeft
  where
    country       = Country "US"
    routingnumber = RoutingNumber "110000000"
    accountnumber = AccountNumber "000123456789"
    name          = Name "David Johnson"
    bankinfo      = NewBankAccount country routingnumber accountnumber
