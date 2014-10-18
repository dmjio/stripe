module Test.Refund where

import           Control.Monad
import           Data.Either
import           Test.Config         (getConfig)
import           Test.Hspec

import           Web.Stripe
import           Web.Stripe.Charge
import           Web.Stripe.Customer
import           Web.Stripe.Refund

------------------------------------------------------------------------------
-- | Card Info
cn :: CardNumber
cn  = CardNumber "4242424242424242"

em :: ExpMonth
em  = ExpMonth 12

ey :: ExpYear
ey  = ExpYear 2015

cvc :: CVC
cvc = CVC "123"

------------------------------------------------------------------------------
-- | Refund Tests
refundTests :: Spec
refundTests = do
      it "Creates a refund succesfully" $ do
        config <- getConfig
        result <- stripe config $ do
          Customer { customerId = cid }  <- createCustomerByCard cn em ey cvc
          Charge   { chargeId   = chid } <- chargeCustomer cid (Currency "usd") 100 Nothing
          refund <- createRefund chid []
          void $ deleteCustomer cid
          return refund
        result `shouldSatisfy` isRight
      it "Retrieves a refund succesfully" $ do
        config <- getConfig
        result <- stripe config $ do
          Customer { customerId = cid  } <- createCustomerByCard cn em ey cvc
          Charge   { chargeId   = chid } <- chargeCustomer cid (Currency "usd") 100 Nothing
          Refund   { refundId   = rid  } <- createRefund chid []
          void $ deleteCustomer cid
          getRefund chid rid
        result `shouldSatisfy` isRight


