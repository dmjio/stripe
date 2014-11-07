{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
module Test.Transfer where

import           Control.Applicative
import qualified Control.Monad as M
import           Data.Aeson (FromJSON)
import           Data.Maybe
import           Data.Either
import           Data.String
import           Prelude (($), IO, Char, fromInteger, error, undefined, id)
import           Test.Hspec
import           Test.Hspec.Core (SpecM)

import           Web.Stripe
import           Web.Stripe.Client
import           Web.Stripe.Recipient
import           Web.Stripe.Transfer

------------------------------------------------------------------------------
-- hack Monad functions to automatically insert callAPI around StripeRequests

void :: (FromJSON a) => StripeRequest a -> Stripe ()
void req = M.void (callAPI req)

class StripeLift a where
  type LiftedType a
  stripeLift :: a -> (LiftedType a)

(>>=) :: (StripeLift t, M.Monad m, LiftedType t ~ m a) =>
         t -> (a -> m b) -> m b
m >>= f = (stripeLift m) M.>>= f

(>>) :: (StripeLift t, M.Monad m, LiftedType t ~ m a) => t -> m b -> m b
(>>) m n = m >>= \_ -> n

fail :: (M.Monad m) => String -> m a
fail = M.fail

return :: (M.Monad m) => a -> m a
return = M.return

instance (FromJSON a) => StripeLift (StripeRequest a) where
  type LiftedType (StripeRequest a) = Stripe a
  stripeLift req = callAPI req

instance (FromJSON a) => StripeLift (Stripe a) where
  type LiftedType (Stripe a) = Stripe a
  stripeLift = id

instance StripeLift (IO a) where
  type LiftedType (IO a) = IO a
  stripeLift = id

instance StripeLift (SpecM a) where
  type LiftedType (SpecM a) = SpecM a
  stripeLift = id

------------------------------------------------------------------------------
-- the tests

transferTests :: StripeConfig -> Spec
transferTests config =
  describe "Transfer tests" $ do

    it "Create a new transfer" $ do
      result <- stripe' config $ do
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
      result <- stripe' config $ do
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
      result <- stripe' config $ do
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
      result <- stripe' config $ do
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
      result <- stripe' config $ do
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
