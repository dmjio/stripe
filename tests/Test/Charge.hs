{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Test.Charge where

import           Control.Monad
import           Control.Monad.IO.Class (liftIO)
import           Data.Either
import           Data.Text              (Text)
import           Test.Config            (getConfig)
import           Test.Hspec
import           Web.Stripe
import           Web.Stripe.Charge
import           Web.Stripe.Customer

chargeTests :: Spec
chargeTests =
  describe "Charge tests" $ do
    it "Charges a customer succesfully" $ do
      config <- getConfig
      result <- stripe config $ do
        Customer { customerId = cid } <- createCustomerByCard cn em ey cvc
        charge <- chargeCustomer cid (Currency "usd") 100 Nothing
        void $ deleteCustomer cid
        return charge
      result `shouldSatisfy` isRight
    it "Retrieves a charge succesfully" $ do
      config <- getConfig
      result <- stripe config $ do
        Customer { customerId = cid } <- createCustomerByCard cn em ey cvc
        Charge { chargeId = chid } <- chargeCustomer cid (Currency "usd") 100 Nothing
        result <- getCharge chid
        void $ deleteCustomer cid
        return result
      result `shouldSatisfy` isRight
    it "Updates a charge succesfully" $ do
      config <- getConfig
      result <- stripe config $ do
        Customer { customerId = cid } <- createCustomerByCard cn em ey cvc
        Charge { chargeId = chid } <- chargeCustomer cid (Currency "usd") 100 Nothing
        _ <- updateCharge chid "Cool" [("hi", "there")]
        result <- getCharge chid
        void $ deleteCustomer cid
        return result
      result `shouldSatisfy` isRight
      let Right Charge { chargeMetaData = cmd, chargeDescription = desc } = result
      cmd `shouldSatisfy` (\x -> ("hi", "there") `elem` x)
      desc `shouldSatisfy` (==(Just "Cool" :: Maybe Text))

  where
    cn  = CardNumber "4242424242424242"
    em  = ExpMonth 9
    ey  = ExpYear 2015
    cvc = CVC "213"
