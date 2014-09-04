{-# LANGUAGE OverloadedStrings #-}

module Web.Stripe.Customer
    ( -- * Types
      Customer  (..)
    , CustomerId(..)
      -- * API Calls
    , createEmptyCustomer
    , createCustomerByEmail
    , createCustomerBase
    , updateCustomer
    , deleteCustomer
    , getCustomer
    ) where

import           Web.Stripe.Client.Internal
import           Web.Stripe.Types

import           Control.Applicative
import           Data.Time

createCustomerByEmail 
    :: Email
    -> Stripe Customer
createCustomerByEmail e = 
    createCustomerBase Nothing Nothing Nothing Nothing 
                       Nothing Nothing Nothing Nothing 
                      (Just e) Nothing Nothing Nothing 

createEmptyCustomer
    :: Stripe Customer
createEmptyCustomer = 
    createCustomerBase Nothing Nothing Nothing Nothing 
                       Nothing Nothing Nothing Nothing 
                       Nothing Nothing Nothing Nothing 

createCustomerBase
    :: Maybe Integer        -- ^ Integer amount
    -> Maybe CardId         -- ^ Either a dictionary of a card or a tokenId
    -> Maybe CardNumber     -- ^ Either a dictionary of a card or a tokenId
    -> Maybe ExpMonth       -- ^ Card Expiration Month
    -> Maybe ExpYear        -- ^ Card Expiration Year
    -> Maybe CVC            -- ^ Card CVC
    -> Maybe CouponId       -- ^ Discount on all recurring charges
    -> Maybe Description    -- ^ Arbitrary string to attach to a customer object
    -> Maybe Email          -- ^ Email address of customer
    -> Maybe PlanId         -- ^ Identifier of plan to subscribe customer to
    -> Maybe Quantity       -- ^ The quantity you'd like to apply to the subscription you're creating
    -> Maybe UTCTime        -- ^ TimeStamp representing the trial period the customer will get
    -> Stripe Customer
createCustomerBase
    accountBalance
    cardId
    cardNumber
    expMonth
    expYear
    cvc
    couponId
    description
    email
    planId
    quantity
    trialEnd = callAPI request
  where request = StripeRequest POST "customers" params
        params  = getParams [
                     ("account_balance", toText <$> accountBalance)
                   , ("card", (\(CardId x) -> x) <$> cardId)
                   , ("card[number]", (\(CardNumber x) -> x) <$> cardNumber)
                   , ("card[exp_month]", (\(ExpMonth x) -> toText x) <$> expMonth)
                   , ("card[exp_year]", (\(ExpYear x) -> toText x) <$> expYear)
                   , ("card[cvc]", (\(CVC x) -> x) <$> cvc)
                   , ("coupon", (\(CouponId x) -> x) <$> couponId)
                   , ("description", (\(Description x) -> x) <$> description)
                   , ("email", (\(Email x) -> x) <$> email)
                   , ("plan", (\(PlanId x) -> x) <$> planId)
                   , ("quantity",  (\(Quantity x) -> toText x) <$> quantity)
                   , ("trial_end", toText <$> trialEnd)
                ]

getCustomer
    :: CustomerId
    -> Stripe Customer
getCustomer (CustomerId cid) = callAPI request
  where request = StripeRequest GET url params
        url     = "customers" </> cid
        params  = []

getCustomers
    :: Stripe (StripeList Customer)
getCustomers = callAPI request
  where request = StripeRequest GET url params
        url     = "customers"
        params  = []

updateCustomer
    :: CustomerId
    -> Stripe Customer
updateCustomer (CustomerId cid) = callAPI request
  where request = StripeRequest POST url params
        url     = "customers" </> cid
        params  = []

deleteCustomer
    :: CustomerId
    -> Stripe Customer
deleteCustomer (CustomerId cid) = callAPI request
  where request = StripeRequest DELETE url params
        url     = "customers" </> cid
        params  = []

