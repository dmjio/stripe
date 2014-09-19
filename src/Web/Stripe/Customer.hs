{-# LANGUAGE OverloadedStrings #-}
module Web.Stripe.Customer
    ( -- * API
      ---- * Create customer
      createEmptyCustomer
    , createCustomerByEmail
    , createCustomerByToken
    , createCustomerByCard
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
      -- * Types
    , Customer           (..)
    , CustomerId         (..)
    , CardId             (..)
    , StripeList         (..)
    , TokenId            (..)
    , CardNumber         (..)
    , ExpMonth           (..)
    , ExpYear            (..)
    , CVC                (..)
    , CouponId           (..)
    , Email              (..)
    , PlanId             (..)
    , Quantity           (..)
    , StripeDeleteResult (..)
    , Description
    , AccountBalance
    , TrialPeriod
    , Limit
    ) where

import           Web.Stripe.Client.Internal (Method (GET, POST, DELETE), Stripe,
                                             StripeRequest (..), callAPI,
                                             getParams, toText, (</>))
import           Web.Stripe.Types           (AccountBalance, CVC (..),
                                             CardNumber (..), CouponId (..),
                                             Customer (..), CustomerId (..), CardId(..),
                                             Description, Email (..), StripeDeleteResult(..),
                                             ExpMonth (..), ExpYear (..), Limit,
                                             PlanId (..), Quantity (..),
                                             StripeList (..), TokenId (..),
                                             TrialPeriod)

------------------------------------------------------------------------------
-- | The base request for customer creation
createCustomerBase
    :: Maybe AccountBalance -- ^ Integer amount corresponding to AccountBalance
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
    -> Maybe TrialPeriod    -- ^ TimeStamp representing the trial period the customer will get
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
                     ("account_balance", toText `fmap` accountBalance)
                   , ("card", (\(TokenId x) -> x) `fmap` cardId)
                   , ("card[number]", (\(CardNumber x) -> x) `fmap` cardNumber)
                   , ("card[exp_month]", (\(ExpMonth x) -> toText x) `fmap` expMonth)
                   , ("card[exp_year]", (\(ExpYear x) -> toText x) `fmap` expYear)
                   , ("card[cvc]", (\(CVC x) -> x) `fmap` cvc)
                   , ("coupon", (\(CouponId x) -> x) `fmap` couponId)
                   , ("description", description)
                   , ("email", (\(Email x) -> x) `fmap` email)
                   , ("plan", (\(PlanId x) -> x) `fmap` planId)
                   , ("quantity",  (\(Quantity x) -> toText x) `fmap` quantity)
                   , ("trial_end", toText `fmap` trialEnd)
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
-- | Creates a 'Customer' with a 'Card'
createCustomerByCard
    :: CardNumber     -- ^ Card Number
    -> ExpMonth       -- ^ Card Expiration Month
    -> ExpYear        -- ^ Card Expiration Year
    -> CVC            -- ^ Card CVC
    -> Stripe Customer
createCustomerByCard
    cardNumber
    expMonth
    expYear
    cvc = createCustomerBase Nothing Nothing (Just cardNumber) (Just expMonth)
                             (Just expYear) (Just cvc) Nothing Nothing
                             Nothing Nothing Nothing Nothing

------------------------------------------------------------------------------
-- | Updates a customer
updateCustomerBase
    :: CustomerId           -- ^ `CustomerId` associated with the `Customer` to update
    -> Maybe AccountBalance -- ^ Integer amount corresponding to AccountBalance
    -> Maybe TokenId        -- ^ Either a dictionary of a card or a tokenId
    -> Maybe CardNumber     -- ^ Either a dictionary of a card or a tokenId
    -> Maybe ExpMonth       -- ^ `Card` Expiration Month
    -> Maybe ExpYear        -- ^ `Card` Expiration Year
    -> Maybe CVC            -- ^ `Card` CVC
    -> Maybe CouponId       -- ^ Discount on all recurring charges
    -> Maybe CardId         -- ^ `CardId` to set as default for customer
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
                    ("account_balance", toText `fmap` accountBalance)
                  , ("card", (\(TokenId x) -> x) `fmap` cardId)
                  , ("card[number]", (\(CardNumber x) -> x) `fmap` cardNumber)
                  , ("card[exp_month]", (\(ExpMonth x) -> toText x) `fmap` expMonth)
                  , ("card[exp_year]", (\(ExpYear x) -> toText x) `fmap` expYear)
                  , ("card[cvc]", (\(CVC x) -> x) `fmap` cvc)
                  , ("coupon", (\(CouponId x) -> x) `fmap` couponId)
                  , ("default_card", (\(CardId x) -> x) `fmap` defaultCardId)
                  , ("description", description)
                  , ("email", (\(Email x) -> x) `fmap` email)
                  ]

------------------------------------------------------------------------------
-- | Update Customer Account Balance
updateCustomerAccountBalance
    :: CustomerId      -- ^ `CustomerId` associated with the `Customer` to update
    -> AccountBalance  -- ^ `AccountBalance` associated
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
    :: CustomerId -- ^ `CustomerId` associated with the `Customer` to update
    -> CardId     -- ^ 'CardId' to become default card
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
        params  = getParams [ ("limit", toText `fmap` limit )]

