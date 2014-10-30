{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Test.Card where

import           Control.Monad
import           Data.Either
import           Data.Maybe

import           Test.Hspec

import           Web.Stripe
import           Web.Stripe.Card
import           Web.Stripe.Customer
import           Web.Stripe.Recipient
import           Web.Stripe.Token

cardTests :: StripeConfig -> Spec
cardTests config = do
    describe "Card tests" $ do
      it "Can create a Customer Card by CardNumber" $ do
        result <- stripe config $ do
          Customer { customerId = cid } <- createEmptyCustomer
          card <- createCustomerCard cid credit em ey cvc
          void $ deleteCustomer cid
          return card
        result `shouldSatisfy` isRight
      it "Can create a Customer Card by TokenId" $ do
        result <- stripe config $ do
          Token    { tokenId = tkid   } <- createCardToken credit em ey cvc
          Customer { customerId = cid } <- createEmptyCustomer
          card <- createCustomerCardByToken cid tkid
          void $ deleteCustomer cid
          return card
        result `shouldSatisfy` isRight
      it "Can retrieve a Customer Card" $ do
        result <- stripe config $ do
          Customer { customerId = customerid
                   , customerCards = StripeList { list = [ Card { cardId = cardid } ] }
                   } <- createCustomerByCard credit em ey cvc
          card <- getCustomerCard customerid cardid
          void $ deleteCustomer customerid
          return card
        result `shouldSatisfy` isRight
        let Right Card{..} = result
        cardLastFour `shouldBe` "4242"
        cardExpMonth `shouldBe` em
        cardExpYear `shouldBe` ey
      it "Can retrieve a Customer's Card with expansion" $ do
        result <- stripe config $ do
          Customer { customerId = customerid
                   , customerCards = StripeList { list = [ Card { cardId = cardid } ] }
                   } <- createCustomerByCard credit em ey cvc
          card <- getCustomerCardExpandable customerid cardid ["customer"]
          void $ deleteCustomer customerid
          return card
        result `shouldSatisfy` isRight
        let Right Card{..} = result
        cardLastFour `shouldBe` "4242"
        cardExpMonth `shouldBe` em
        cardExpYear `shouldBe` ey
      it "Can retrieve a Customer's Cards" $ do
        result <- stripe config $ do
          Customer { customerId = customerid
                   } <- createCustomerByCard credit em ey cvc
          card <- getCustomerCards customerid Nothing Nothing Nothing
          void $ deleteCustomer customerid
          return card
        result `shouldSatisfy` isRight
      it "Can retrieve a Customer's Cards with Expansion" $ do
        result <- stripe config $ do
          Customer { customerId = customerid
                   } <- createCustomerByCard credit em ey cvc
          card <- getCustomerCardsExpandable customerid Nothing Nothing Nothing ["data.customer"]
          void $ deleteCustomer customerid
          return card
        result `shouldSatisfy` isRight
      it "Can delete a Customer's Cards" $ do
        result <- stripe config $ do
          Customer { customerId = customerid
                    , customerDefaultCard = Just cardid
                   } <- createCustomerByCard credit em ey cvc
          result <- deleteCustomerCard customerid cardid
          void $ deleteCustomer customerid
          return result
        result `shouldSatisfy` isRight
      it "Can update a Customer's Card" $ do
        result <- stripe config $ do
          Customer { customerId = customerid
                    , customerDefaultCard = Just cardid
                   } <- createCustomerByCard credit em ey cvc
          result <- updateCustomerCard customerid cardid
                       cardname
                       cardcity
                       cardcountry
                       cardaddressOne
                       cardaddressTwo
                       cardaddressState
                       cardzip
          void $ deleteCustomer customerid
          return result
        result `shouldSatisfy` isRight
        let Right Card{..} = result
        cardName `shouldBe` cardname
        cardAddressCity `shouldBe` cardcity
        cardAddressCountry `shouldBe` cardcountry
        cardAddressLine1 `shouldBe` cardaddressOne
        cardAddressLine2 `shouldBe` cardaddressTwo
        cardAddressState `shouldBe` cardaddressState
        cardAddressZip `shouldBe` cardzip
    describe "Recipient Card tests" $ do
      it "Can create a RecipientCard by CardNumber" $ do
        result <- stripe config $ do
          r@Recipient{..} <- createRecipientByCard firstname lastname Nothing Individual debit em ey cvc
          void $ deleteRecipient recipientId
          return r
        result `shouldSatisfy` isRight
        let Right Recipient {..} = result
        length (list recipientCards) `shouldBe` 1
      it "Can create a RecipientCard by Card TokenId" $ do
        result <- stripe config $ do
          Token { tokenId = tkid } <- createCardToken debit em ey cvc
          Recipient { recipientId = rid } <- createRecipient firstname lastname (Just 'M') Individual
          rcard <- createRecipientCardByToken rid tkid
          void $ deleteRecipient rid
          return rcard
        result `shouldSatisfy` isRight
      it "Fails to create a RecipientCard by BankAccount TokenId" $ do
        result <- stripe config $ do
          Token { tokenId = tkid } <- createBankAccountToken country routingnumber accountnumber
          Recipient { recipientId = rid } <- createRecipient firstname lastname (Just 'M') Corporation
          rcard <- createRecipientCardByToken rid tkid
          void $ deleteRecipient rid
          return rcard
        result `shouldSatisfy` isLeft
      it "Can retrieve a RecipientCard" $ do
        result <- stripe config $ do
          Recipient{..} <- createRecipientByCard firstname lastname Nothing Individual debit em ey cvc
          rcard <- getRecipientCard recipientId (fromJust recipientDefaultCard)
          void $ deleteRecipient recipientId
          return rcard
        result `shouldSatisfy` isRight
      it "Can retrieve a RecipientCard Expanded" $ do
        result <- stripe config $ do
          Recipient{..} <- createRecipientByCard firstname lastname Nothing Individual debit em ey cvc
          rcard <- getRecipientCardExpandable recipientId (fromJust recipientDefaultCard) ["recipient"]
          void $ deleteRecipient recipientId
          return rcard
        result `shouldSatisfy` isRight
      it "Can retrieve a Recipient's Cards" $ do
        result <- stripe config $ do
          Recipient{..} <- createRecipientByCard firstname lastname Nothing Individual debit em ey cvc
          rcard <- getRecipientCards recipientId Nothing Nothing Nothing
          void $ deleteRecipient recipientId
          return rcard
        result `shouldSatisfy` isRight
      it "Can retrieve a Recipient's Cards Expanded" $ do
        result <- stripe config $ do
          Recipient{..} <- createRecipientByCard firstname lastname Nothing Individual debit em ey cvc
          rcard <- getRecipientCardsExpandable recipientId Nothing Nothing Nothing ["data.recipient"]
          void $ deleteRecipient recipientId
          return rcard
        result `shouldSatisfy` isRight
      it "Can delete a Recipient Card Expanded" $ do
        result <- stripe config $ do
          Recipient{..} <- createRecipientByCard firstname lastname Nothing Individual debit em ey cvc
          rcard <- deleteRecipientCard recipientId (fromJust recipientDefaultCard)
          void $ deleteRecipient recipientId
          return rcard
        result `shouldSatisfy` isRight
      it "Can update a Recipient's Card" $ do
        result <- stripe config $ do
          Recipient{..} <- createRecipientByCard firstname lastname Nothing Individual debit em ey cvc
          rcard@RecipientCard{..} <-
                       updateRecipientCard
                       recipientId
                       (fromJust recipientDefaultCard)
                       cardname
                       cardcity
                       cardcountry
                       cardaddressOne
                       cardaddressTwo
                       cardaddressState
                       cardzip
          void $ deleteRecipient recipientId
          return rcard
        result `shouldSatisfy` isRight
        let Right RecipientCard{..} = result
        recipientCardName `shouldBe` cardname
        recipientCardAddressCity `shouldBe` cardcity
        recipientCardAddressCountry `shouldBe` cardcountry
        recipientCardAddressLine1 `shouldBe` cardaddressOne
        recipientCardAddressLine2 `shouldBe` cardaddressTwo
        recipientCardAddressState `shouldBe` cardaddressState
        recipientCardAddressZip `shouldBe` cardzip

      it "Fails to add a Credit Card to a Recipient" $ do
        result <- stripe config $ 
          createRecipientByCard firstname lastname Nothing Individual credit em ey cvc
        result `shouldSatisfy` isLeft

  where
    credit = CardNumber "4242424242424242"
    debit  = CardNumber "4000056655665556"
    em  = ExpMonth 12
    ey  = ExpYear 2015
    cvc = CVC "123"
    country = Country "US"
    routingnumber = RoutingNumber "110000000"
    accountnumber = AccountNumber "000123456789"
    firstname = FirstName "David"
    lastname = LastName "Johnson"
    cardname = Just "cardName"
    cardcity = Just (AddressCity "Chicago")
    cardcountry = Just (AddressCountry "US")
    cardaddressOne = Just (AddressLine1 "123 Fake Street")
    cardaddressTwo = Just (AddressLine2 "456 Fake Street")
    cardaddressState = Just (AddressState "IL")
    cardzip = Just (AddressZip "60610")




