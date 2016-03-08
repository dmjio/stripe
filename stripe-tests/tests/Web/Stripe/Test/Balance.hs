{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax  #-}
module Web.Stripe.Test.Balance where

import           Data.Either
import           Test.Hspec
import           Web.Stripe.Test.Prelude

import           Web.Stripe.Balance
import           Web.Stripe.Charge
import           Web.Stripe.Customer
import           Web.Stripe.StripeRequest (Expandable (..))

balanceTests :: StripeSpec
balanceTests stripe = do
  describe "Balance tests" $ do
    it "Succesfully retrieves a Balance" $ do
      result <- stripe $ do b <- getBalance
                            return b
      result `shouldSatisfy` isRight
    it "Succesfully retrieves a Balance Transaction" $ do
      result <- stripe $ do
          Customer { customerId = cid } <-
            createCustomer
              -&- ((mkNewCard cn em ey) { newCardCVC = Just cvc })
          Charge { chargeBalanceTransaction = Just (Id txid) } <-
            createCharge (Amount 100) USD -&- cid
          balance <- getBalanceTransaction txid
          void $ deleteCustomer cid
          return balance
      result `shouldSatisfy` isRight
    it "Succesfully retrieves an Expanded Balance Transaction" $ do
       result <- stripe $ do
          Customer { customerId = cid } <-
            createCustomer
              -&- ((mkNewCard cn em ey) { newCardCVC = Just cvc })
          Charge   { chargeBalanceTransaction = Just (Id txid)
                   } <- createCharge (Amount 100) USD -&- cid
          result <- getBalanceTransaction txid -&- ExpandParams ["source"]
          void $ deleteCustomer cid
          return result
       result `shouldSatisfy` isRight
    it "Succesfully retrieves Balance Transaction History" $ do
      result <- stripe $ do b <- getBalanceTransactionHistory
                            return b
      result `shouldSatisfy` isRight
  where
    cn  = CardNumber "4242424242424242"
    em  = ExpMonth 12
    ey  = ExpYear 2020
    cvc = CVC "123"

