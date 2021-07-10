{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax  #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE RankNTypes #-}
module Web.Stripe.Test.Token where

import           Data.Either
import           Test.Hspec
import           Web.Stripe.Test.Prelude
import           Web.Stripe.Token

tokenTests :: StripeSpec
tokenTests stripe = do
  describe "Token tests" $ do
    it "Can create a Card Token" $ do
      result <- stripe $ void $ createCardToken (Just cardinfo)
      result `shouldSatisfy` isRight
    it "Can create a Bank Account Token" $ do
      result <- stripe $ void $ createBankAccountToken
                                  (Just bankinfo)
      result `shouldSatisfy` isRight
    it "Can retrieve an Existing Card Token" $ do
      result <- stripe $ do
        Token { tokenId = tkid } <- createCardToken (Just cardinfo)
        void $ getCardToken tkid
      result `shouldSatisfy` isRight
    it "Can retrieve an Existing Bank Account Token" $ do
      result <- stripe $ do
        Token { tokenId = tkid } <- createBankAccountToken (Just bankinfo)
        void $ getBankAccountToken tkid
      result `shouldSatisfy` isRight
  where
    cn  = CardNumber "4242424242424242"
    em  = ExpMonth 12
    ey  = ExpYear 2025
    cvc = CVC "123"
    cardinfo = (mkNewCard cn em ey) { newCardCVC = Just cvc }
    bankinfo = NewBankAccount
                  (Country "US")
                  (RoutingNumber "110000000")
                  (AccountNumber "000123456789")
