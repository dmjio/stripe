{-# LANGUAGE OverloadedStrings #-}

module Web.Stripe.Customer
    ( -- * Types
      Customer(..)
    , CustomerId(..)
      -- * API Calls
--    , createCustomer
    , updateCustomer
    , deleteCustomer
    , getCustomer
    ) where

import           Web.Stripe.Client.Internal
import           Web.Stripe.Client.Util
import           Web.Stripe.Types


-- createCustomer = 
--     createCustomerBase 
--       Nothing Nothing Nothing Nothing 
--       Nothing Nothing Nothing Nothing 
--       Nothing Nothing Nothing Nothing 
--       Nothing

-- createCustomerBase ::
--     Maybe AccountBalance -> -- ^ Integer amount
--     Maybe Card -> -- ^ Either a dictionary of a card or a tokenId
--     Maybe TokenId -> -- ^ Either a dictionary of a card or a tokenId
--     Maybe CouponId -> -- ^ Discount on all recurring charges
--     Maybe Description -> -- ^ Arbitrary string to attach to a customer object
--     Maybe Email -> -- ^ Email address of customer
--     Maybe PlanId -> -- ^ Identifier of plan to subscribe customer to
--     Maybe Quantity -> -- ^ The quantity you'd like to apply to the subscription you're creating
--     Maybe TrialEnd -> -- ^ TimeStamp representing the trial period the customer will get
--     Stripe Customer
-- createCustomerBase = 
--     accountBalance
--     card
--     tokenId
--     couponId
--     description
--     email
--     plan
--     quantity
--     trialEnd
--     callAPI req
--   where req = StripeRequest POST "customers" params
--         params = [ (x, y) | (x, Just y) <- [
--                      ("account_balance", fmap toBS accountBalance)
--                    , ("card", (\(TokenId tkid) -> T.encodeUtf8 tkid) <$> tokenId)
--                    -- , ("card[number]", (\(CardNumber c) -> toBS c) <$> cardNumber)
--                    -- , ("card[exp_month]", (\(ExpMonth m) -> toBS m) <$> expMonth)
--                    -- , ("card[exp_year]", (\(ExpYear y) -> toBS y) <$> expYear)
--                    -- , ("card[cvc]", (\(CVC cvc) -> toBS cvc) <$> cvc)
--                    , ("coupon", (\(CouponId x) -> T.encodeUtf8 x) <$> couponId)
--                    , ("description", fmap T.encodeUtf8 description)
--                    , ("email", (\(Email x) -> T.encodeUtf8 x) <$> email)
--                    , ("plan", (\(PlanId x) -> T.encodeUtf8 x) <$> planId)
--                    , ("quantity", T.encodeUtf8 quantity)
--                    , ("trial_end", fmap toBS customerTrialEndOptions)
--                    ]
--                 ]

getCustomer :: CustomerId -> Stripe Customer
getCustomer (CustomerId cid) = callAPI request
  where request = StripeRequest GET url params
        url     = "customers" </> cid
        params  = []

updateCustomer :: CustomerId -> Stripe Customer
updateCustomer (CustomerId cid) = callAPI request
  where request = StripeRequest POST url params
        url     = "customers" </> cid
        params  = []

deleteCustomer :: CustomerId -> Stripe Customer
deleteCustomer (CustomerId cid) = callAPI request
  where request = StripeRequest DELETE url params
        url     = "customers" </> cid
        params  = []

