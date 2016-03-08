{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax  #-}
{-# LANGUAGE RecordWildCards   #-}
module Web.Stripe.Test.Card where

import           Data.Either
import           Data.Maybe
import           Test.Hspec
import           Web.Stripe.Card
import           Web.Stripe.Customer
import           Web.Stripe.Recipient
import           Web.Stripe.StripeRequest (Expandable (Id))
import           Web.Stripe.Test.Prelude
import           Web.Stripe.Token

cardTests :: StripeSpec
cardTests stripe = do
    describe "Card tests" $ do
      it "Can create a Customer and add a Card by CardNumber" $ do
        result <- stripe $ do
          Customer { customerId = cid } <- createCustomer
          card <- createCustomerCard cid cardinfo
          void $ deleteCustomer cid
          return card
        result `shouldSatisfy` isRight

      it "Can create a Customer Card by TokenId" $ do
        result <- stripe $ do
          Token    { tokenId = tkid   } <- createCardToken (Just cardinfo)
          Customer { customerId = cid } <- createCustomer
          card <- createCustomerCardByToken cid tkid
          void $ deleteCustomer cid
          return card
        result `shouldSatisfy` isRight

      it "Can retrieve a Customer Card" $ do
        result <- stripe $ do
          Customer { customerId = customerid
                   , customerCards = StripeList { list = [ Card { cardId = cardid } ] }
                   } <- createCustomer -&- cardinfo
          card <- getCustomerCard customerid cardid
          void $ deleteCustomer customerid
          return card
        result `shouldSatisfy` isRight
        let Right Card{..} = result
        cardLastFour `shouldBe` "4242"
        cardExpMonth `shouldBe` em
        cardExpYear `shouldBe` ey

      it "Can retrieve a Customer's Card with expansion" $ do
        result <- stripe $ do
          Customer { customerId = customerid
                   , customerCards = StripeList { list = [ Card { cardId = cardid } ] }
                   } <- createCustomer -&- cardinfo
          card <- getCustomerCard customerid cardid -&- ExpandParams ["customer"]
          void $ deleteCustomer customerid
          return card
        result `shouldSatisfy` isRight
        let Right Card{..} = result
        cardLastFour `shouldBe` "4242"
        cardExpMonth `shouldBe` em
        cardExpYear `shouldBe` ey

      it "Can retrieve a Customer's Cards" $ do
        result <- stripe $ do
          Customer { customerId = customerid
                   } <- createCustomer -&- cardinfo
          card <- getCustomerCards customerid
          void $ deleteCustomer customerid
          return card
        result `shouldSatisfy` isRight

      it "Can retrieve a Customer's Cards with Expansion" $ do
        result <- stripe $ do
          Customer { customerId = customerid
                   } <- createCustomer -&- cardinfo
          card <- getCustomerCards customerid -&- ExpandParams ["data.customer"]
          void $ deleteCustomer customerid
          return card
        result `shouldSatisfy` isRight

      it "Can delete a Customer's Cards" $ do
        result <- stripe $ do
          Customer { customerId = customerid
                    , customerDefaultCard = Just (Id cardid)
                   } <- createCustomer -&- cardinfo
          result <- deleteCustomerCard customerid cardid
          void $ deleteCustomer customerid
          return result
        result `shouldSatisfy` isRight

      it "Can update a Customer's Card" $ do
        result <- stripe $ do
          Customer { customerId = customerid
                    , customerDefaultCard = Just (Id cardid)
                   } <- createCustomer -&- cardinfo
          result <- updateCustomerCard customerid cardid
                       -&- cardname
                       -&- cardcity
                       -&- cardcountry
                       -&- cardaddressOne
                       -&- cardaddressTwo
                       -&- cardaddressState
                       -&- cardzip
          void $ deleteCustomer customerid
          return result
        result `shouldSatisfy` isRight
        let Right Card{..} = result
        cardName           `shouldBe` (Just cardname)
        cardAddressCity    `shouldBe` (Just cardcity)
        cardAddressCountry `shouldBe` (Just cardcountry)
        cardAddressLine1   `shouldBe` (Just cardaddressOne)
        cardAddressLine2   `shouldBe` (Just cardaddressTwo)
        cardAddressState   `shouldBe` (Just cardaddressState)
        cardAddressZip     `shouldBe` (Just cardzip)

    describe "Recipient Card tests" $ do
      it "Can create a RecipientCard by CardNumber" $ do
        result <- stripe $ do
          r@Recipient{..} <- createRecipient name Individual -&- debitinfo
          void $ deleteRecipient recipientId
          return r
        result `shouldSatisfy` isRight
        let Right Recipient {..} = result
        length (list recipientCards) `shouldBe` 1

      it "Can create a RecipientCard by Card TokenId" $ do
        result <- stripe $ do
          Token { tokenId = tkid } <- createCardToken (Just debitinfo)
          Recipient { recipientId = rid } <- createRecipient name Individual
          rcard <- createRecipientCardByToken rid tkid
          void $ deleteRecipient rid
          return rcard
        result `shouldSatisfy` isRight

      it "Fails to create a RecipientCard by BankAccount TokenId" $ do
        result <- stripe $ do
          Token { tokenId = tkid } <- createBankAccountToken (Just $ NewBankAccount country routingnumber accountnumber)
          Recipient { recipientId = rid } <- createRecipient name Corporation
          rcard <- createRecipientCardByToken rid tkid
          void $ deleteRecipient rid
          return rcard
        result `shouldSatisfy` isLeft

      it "Can retrieve a RecipientCard" $ do
        result <- stripe $ do
          Recipient{..} <- createRecipient name Individual -&- debitinfo
          let (Id cardid) = fromJust recipientDefaultCard
          rcard <- getRecipientCard recipientId cardid
          void $ deleteRecipient recipientId
          return rcard
        result `shouldSatisfy` isRight

      it "Can retrieve a RecipientCard Expanded" $ do
        result <- stripe $ do
          Recipient{..} <- createRecipient name Individual -&- debitinfo
          let (Id cardid) = fromJust recipientDefaultCard
          rcard <- getRecipientCard recipientId cardid -&- ExpandParams ["recipient"]
          void $ deleteRecipient recipientId
          return rcard
        result `shouldSatisfy` isRight

      it "Can retrieve a Recipient's Cards" $ do
        result <- stripe $ do
          Recipient{..} <- createRecipient name Individual -&- debitinfo
          rcard <- getRecipientCards recipientId
          void $ deleteRecipient recipientId
          return rcard
        result `shouldSatisfy` isRight

      it "Can retrieve a Recipient's Cards Expanded" $ do
        result <- stripe $ do
          Recipient{..} <- createRecipient name Individual -&- debitinfo
          rcard <- getRecipientCards recipientId -&- ExpandParams  ["data.recipient"]
          void $ deleteRecipient recipientId
          return rcard
        result `shouldSatisfy` isRight

      it "Can delete a Recipient Card" $ do
        result <- stripe $ do
          Recipient{..} <- createRecipient name Individual -&- debitinfo
          let (Id defaultCard) = fromJust recipientDefaultCard
          rcard <- deleteRecipientCard recipientId defaultCard
          void $ deleteRecipient recipientId
          return rcard
        result `shouldSatisfy` isRight

      it "Can update a Recipient's Card" $ do
        result <- stripe $ do
          Recipient{..} <- createRecipient name Individual -&- debitinfo
          let (Id cardid) = fromJust recipientDefaultCard
          rcard@RecipientCard{..} <-
                       updateRecipientCard recipientId cardid
                        -&- cardname
                        -&- cardcity
                        -&- cardcountry
                        -&- cardaddressOne
                        -&- cardaddressTwo
                        -&- cardaddressState
                        -&- cardzip
          void $ deleteRecipient recipientId
          return rcard
        result `shouldSatisfy` isRight
        let Right RecipientCard{..} = result
        recipientCardName `shouldBe` (Just cardname)
        recipientCardAddressCity `shouldBe` (Just cardcity)
        recipientCardAddressCountry `shouldBe` (Just cardcountry)
        recipientCardAddressLine1 `shouldBe` (Just cardaddressOne)
        recipientCardAddressLine2 `shouldBe` (Just cardaddressTwo)
        recipientCardAddressState `shouldBe` (Just cardaddressState)
        recipientCardAddressZip `shouldBe` (Just cardzip)

      it "Fails to add a Credit Card to a Recipient" $ do
        result <- stripe $ do
          r <- createRecipient name Individual -&- cardinfo
          void $ deleteRecipient (recipientId r)
          return r
        result `shouldSatisfy` isLeft

  where
    cardinfo = (mkNewCard credit em ey) { newCardCVC = Just cvc }
    debitinfo = (mkNewCard debit em ey) { newCardCVC = Just cvc }
    credit = CardNumber "4242424242424242"
    debit  = CardNumber "4000056655665556"
    em  = ExpMonth 12
    ey  = ExpYear 2020
    cvc = CVC "123"
    country = Country "US"
    routingnumber = RoutingNumber "110000000"
    accountnumber = AccountNumber "000123456789"
    name = Name "David Johnson"
    cardname = Name "cardName"
    cardcity         = AddressCity "Chicago"
    cardcountry      = AddressCountry "US"
    cardaddressOne   = AddressLine1 "123 Fake Street"
    cardaddressTwo   = AddressLine2 "456 Fake Street"
    cardaddressState = AddressState "IL"
    cardzip          = AddressZip "60610"
