{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RebindableSyntax #-}
module Test.Token where

import           Data.Either
import           Test.Hspec
import           Test.Prelude
import           Web.Stripe.Token

tokenTests :: StripeSpec
tokenTests stripe = do
  describe "Token tests" $ do
    it "Can create a Card Token" $ do
      result <- stripe $ void $ createCardToken cn em ey cvc
      result `shouldSatisfy` isRight
    it "Can create a Bank Account Token" $ do
      result <- stripe $ void $ createBankAccountToken
                                  (Country "US")
                                  (RoutingNumber "110000000")
                                  (AccountNumber "000123456789")
      result `shouldSatisfy` isRight
    it "Can retrieve an Existing Card Token" $ do
      result <- stripe $ do
        Token { tokenId = tkid } <- createCardToken cn em ey cvc
        void $ getCardToken tkid
      result `shouldSatisfy` isRight
    it "Can retrieve an Existing Bank Account Token" $ do
      result <- stripe $ do
        Token { tokenId = tkid } <- createBankAccountToken
                 (Country "US")
                 (RoutingNumber "110000000")
                 (AccountNumber "000123456789")
        void $ getBankAccountToken tkid
      result `shouldSatisfy` isRight
  where
    cn  = CardNumber "4242424242424242"
    em  = ExpMonth 12
    ey  = ExpYear 2015
    cvc = CVC "123"
