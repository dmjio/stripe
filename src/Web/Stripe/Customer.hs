{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Web.Stripe.Customer
-- Copyright   : (c) David Johnson, 2014
-- License     : BSD3
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX

module Web.Stripe.Customer
    ( -- * Customer Types
      Customer   (..) 
    , CustomerId (..)
    , StripeList (..)
      -- * API Calls
      ---- * Create customer 
    , createEmptyCustomer
    , createCustomerByEmail
    , createCustomerByToken
    , createCustomerBase
      ---- * Update customer
    , updateCustomerBase
    , updateCustomerAccountBalance
    , updateCustomerDefaultCard
      ---- * Delete customer
    , deleteCustomer
      ---- * Get customer(s)
    , getCustomer
    , getCustomers
    ) where

import           Control.Applicative
import           Data.Time

import           Web.Stripe.Client.Internal
import           Web.Stripe.Types

------------------------------------------------------------------------------
-- | The base request for customer creation
createCustomerBase
    :: Maybe Integer        -- ^ Integer amount
    -> Maybe TokenId        -- ^ Either a dictionary of a card or a 'TokenId'
    -> Maybe CardNumber     -- ^ Card Number
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
                   , ("card", (\(TokenId x) -> x) <$> cardId)
                   , ("card[number]", (\(CardNumber x) -> x) <$> cardNumber)
                   , ("card[exp_month]", (\(ExpMonth x) -> toText x) <$> expMonth)
                   , ("card[exp_year]", (\(ExpYear x) -> toText x) <$> expYear)
                   , ("card[cvc]", (\(CVC x) -> x) <$> cvc)
                   , ("coupon", (\(CouponId x) -> x) <$> couponId)
                   , ("description", description)
                   , ("email", (\(Email x) -> x) <$> email)
                   , ("plan", (\(PlanId x) -> x) <$> planId)
                   , ("quantity",  (\(Quantity x) -> toText x) <$> quantity)
                   , ("trial_end", toText <$> trialEnd)
                ]

------------------------------------------------------------------------------
-- | Creates a customer by his/her email
createCustomerByEmail
    :: Email
    -> Stripe Customer
createCustomerByEmail e =
    createCustomerBase Nothing Nothing Nothing Nothing
                       Nothing Nothing Nothing Nothing
                      (Just e) Nothing Nothing Nothing

------------------------------------------------------------------------------
-- | Creates a blank customer
createEmptyCustomer
    :: Stripe Customer
createEmptyCustomer =
    createCustomerBase Nothing Nothing Nothing Nothing
                       Nothing Nothing Nothing Nothing
                       Nothing Nothing Nothing Nothing

------------------------------------------------------------------------------
-- | Creates a customer by a Token created from stripe.js or the stripe API.
createCustomerByToken
    :: TokenId
    -> Stripe Customer
createCustomerByToken t =
    createCustomerBase Nothing (Just t) Nothing Nothing
                       Nothing Nothing Nothing Nothing
                       Nothing Nothing Nothing Nothing

------------------------------------------------------------------------------
-- | Retrieves a customer by his/her ID.
getCustomer
    :: CustomerId
    -> Stripe Customer
getCustomer (CustomerId cid) = callAPI request
  where request = StripeRequest GET url params
        url     = "customers" </> cid
        params  = []

------------------------------------------------------------------------------
-- | Retrieve up to 100 customers at a time
getCustomers
    :: Maybe Limit
    -> Stripe (StripeList Customer)
getCustomers limit
    = callAPI request
  where request = StripeRequest GET url params
        url     = "customers"
        params  = getParams [ ("limit", toText <$> limit )]

------------------------------------------------------------------------------
-- | Updates a customer
updateCustomerBase
    :: CustomerId
    -> Maybe AccountBalance -- ^ Integer amount
    -> Maybe TokenId        -- ^ Either a dictionary of a card or a tokenId
    -> Maybe CardNumber     -- ^ Either a dictionary of a card or a tokenId
    -> Maybe ExpMonth       -- ^ Card Expiration Month
    -> Maybe ExpYear        -- ^ Card Expiration Year
    -> Maybe CVC            -- ^ Card CVC
    -> Maybe CouponId       -- ^ Discount on all recurring charges
    -> Maybe CardId         -- ^ CardID to set as default for customer
    -> Maybe Description    -- ^ Arbitrary string to attach to a customer object
    -> Maybe Email          -- ^ Email address of customer
    -> Stripe Customer
updateCustomerBase
    (CustomerId cid)
    accountBalance
    cardId
    cardNumber
    expMonth
    expYear
    cvc
    couponId
    defaultCardId
    description
    email       = callAPI request
  where request = StripeRequest POST url params
        url     = "customers" </> cid
        params  = getParams [ 
                    ("account_balance", toText <$> accountBalance) 
                  , ("card", (\(TokenId x) -> x) <$> cardId)
                  , ("card[number]", (\(CardNumber x) -> x) <$> cardNumber)
                  , ("card[exp_month]", (\(ExpMonth x) -> toText x) <$> expMonth)
                  , ("card[exp_year]", (\(ExpYear x) -> toText x) <$> expYear)
                  , ("card[cvc]", (\(CVC x) -> x) <$> cvc)
                  , ("coupon", (\(CouponId x) -> x) <$> couponId)
                  , ("default_card", (\(CardId x) -> x) <$> defaultCardId)
                  , ("description", description)
                  , ("email", (\(Email x) -> x) <$> email)
                  ]

------------------------------------------------------------------------------
-- | Update Customer Account Balance
updateCustomerAccountBalance
    :: CustomerId
    -> AccountBalance
    -> Stripe Customer
updateCustomerAccountBalance
    customerId
    accountBalance 
        = updateCustomerBase customerId (Just accountBalance)
            Nothing Nothing Nothing
            Nothing Nothing Nothing
            Nothing Nothing Nothing

------------------------------------------------------------------------------
-- | Update Customer Account Balance
updateCustomerDefaultCard
    :: CustomerId
    -> CardId -- ^ 'CardId' to become default card 
    -> Stripe Customer
updateCustomerDefaultCard
    customerId
    defaultCard
        = updateCustomerBase customerId Nothing
            Nothing Nothing Nothing
            Nothing Nothing Nothing
            (Just defaultCard) Nothing Nothing


------------------------------------------------------------------------------
-- | Deletes the specified customer
deleteCustomer
    :: CustomerId
    -> Stripe StripeDeleteResult
deleteCustomer (CustomerId cid) = callAPI request
  where request = StripeRequest DELETE url params
        url     = "customers" </> cid
        params  = []

