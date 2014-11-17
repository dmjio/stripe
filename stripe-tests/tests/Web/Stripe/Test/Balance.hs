{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
module Web.Stripe.Test.Balance where

import           Data.Either
import           Test.Hspec
import           Web.Stripe.Test.Prelude

import           Web.Stripe.Balance
import           Web.Stripe.Charge
import           Web.Stripe.Customer

balanceTests :: StripeSpec
balanceTests stripe = do
  describe "Balance tests" $ do
    it "Succesfully retrieves a Balance" $ do
      result <- stripe $ do b <- getBalance
                            return b
      result `shouldSatisfy` isRight
    it "Succesfully retrieves a Balance Transaction" $ do
      result <- stripe $ do
          Customer { customerId = cid } <- createCustomerByCard cn em ey cvc
          Charge { chargeBalanceTransaction = Just txid } <-
            chargeCustomer cid USD 100 Nothing
          balance <- getBalanceTransaction txid
          void $ deleteCustomer cid
          return balance
      result `shouldSatisfy` isRight
    it "Succesfully retrieves an Expanded Balance Transaction" $ do
       result <- stripe $ do
          Customer { customerId = cid } <- createCustomerByCard cn em ey cvc
          Charge   { chargeBalanceTransaction = Just txid
                   } <- chargeCustomer cid USD 100 Nothing
          result <- getBalanceTransactionExpandable txid ["source"]
          void $ deleteCustomer cid
          return result
       result `shouldSatisfy` isRight
    it "Succesfully retrieves Balance Transaction History" $ do
      result <- stripe $ do b <- getBalanceTransactionHistory Nothing Nothing Nothing
                            return b
      result `shouldSatisfy` isRight
  where
    cn  = CardNumber "4242424242424242"
    em  = ExpMonth 12
    ey  = ExpYear 2015
    cvc = CVC "123"






