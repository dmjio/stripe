{-# LANGUAGE OverloadedStrings #-}
-------------------------------------------
-- |
-- Module      : Web.Stripe.Charge
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- < https:/\/\stripe.com/docs/api#charges >
--
-- @
-- import Web.Stripe         
-- import Web.Stripe.Customer 
-- import Web.Stripe.Charge
--
-- main :: IO ()
-- main = do
--   let config = SecretKey "secret_key"
--       credit = CardNumber "4242424242424242"
--       em  = ExpMonth 12
--       ey  = ExpYear 2015
--       cvc = CVC "123"
--   result <- stripe config $ do
--         Customer { customerId = cid } <- createCustomerByCard cn em ey cvc
--         charge <- chargeCustomer cid USD 100 Nothing
--         return charge
--   case result of
--     Right charge      -> print charge
--     Left  stripeError -> print stripeError
-- @
module Web.Stripe.Charge
    ( -- * API
      ---- * Create Charges
      chargeCustomer
    , chargeCardByToken
    , chargeCustomerByCardId
    , chargeCard
    , chargeBase
      ---- * Get Charge(s)
    , getCharge
    , getChargeExpandable
    , getCharges
    , getChargesExpandable
    , getCustomerCharges
    , getCustomerChargesExpandable
      ---- * Update Charge
    , updateCharge
      ---- * Capture Charge
    , captureCharge
      -- * Types
    , Charge       (..)
    , TokenId      (..)
    , ChargeId     (..)
    , CustomerId   (..)
    , Customer     (..)
    , Currency     (..)
    , CardNumber   (..)
    , CVC          (..)
    , ExpMonth     (..)
    , ExpYear      (..)
    , StripeList   (..)
    , Email        (..)
    , Description
    , StatementDescription
    , Amount
    , Capture
    ) where

import           Web.Stripe.Client.Internal (Method (GET, POST), Stripe,
                                             StripeRequest (..), callAPI,
                                             getParams, toMetaData, toText, toExpandable,
                                             toTextLower, (</>))
import           Web.Stripe.Types           (Amount, CVC (..), Capture, 
                                             CardNumber (..), Charge (..),
                                             ChargeId (..), Currency (..),
                                             CustomerId (..), Description,
                                             EndingBefore, ExpMonth (..),
                                             ExpYear (..), Limit, MetaData,
                                             Email (..), StartingAfter, Customer(..),
                                             StatementDescription(..), ExpandParams,
                                             StripeList (..), TokenId (..), CardId(..))
import           Web.Stripe.Types.Util      (getCardId, getChargeId, getCustomerId)

------------------------------------------------------------------------------
-- | Charge `Customer``s by `CustomerId`, will charge the default `Card` if exists
chargeCustomer
    :: CustomerId   -- ^ The `CustomerId` of the `Customer` to be charged
    -> Currency     -- ^ Required, 3-letter ISO Code
    -> Amount       -- ^ Required, Integer value of 100 represents $1
    -> Maybe Description -- ^ Optional, default is null
    -> Stripe Charge
chargeCustomer customerid currency amount description =
    chargeBase amount currency description (Just customerid)
    Nothing Nothing Nothing True
    Nothing Nothing Nothing Nothing []

------------------------------------------------------------------------------
-- | Charge `Customer`s by `CustomerId`
chargeCustomerByCardId
    :: CustomerId   -- ^ The `CustomerId` of the `Customer` to be charged
    -> CardId       -- ^ `CardId` of `Customer` to charge
    -> Currency     -- ^ Required, 3-letter ISO Code
    -> Amount       -- ^ Required, Integer value of 100 represents $1
    -> Maybe Description -- ^ Optional, default is null
    -> Stripe Charge
chargeCustomerByCardId
    customerid
    cardid
    currency
    amount
    description =
      chargeBase amount currency description
      (Just customerid) (Just $ TokenId $ getCardId cardid)  Nothing
      Nothing True Nothing Nothing
      Nothing Nothing []

------------------------------------------------------------------------------
-- | Charge a card by a `TokenId`
chargeCardByToken
    :: TokenId    -- ^ The `TokenId` representative of a `Card`
    -> Currency   -- ^ Required, 3-letter ISO Code
    -> Amount     -- ^ Required, Integer value of 100 represents $1
    -> Maybe Description -- ^ Optional, default is null
    -> Stripe Charge
chargeCardByToken tokenId currency amount description =
    chargeBase amount currency description Nothing (Just tokenId)
    Nothing Nothing True Nothing Nothing Nothing Nothing []

------------------------------------------------------------------------------
-- | Charge a card by `CardNumber`
chargeCard
    :: CardNumber        -- ^ Required, Credit Card Number
    -> ExpMonth          -- ^ Required, Expiration Month (i.e. 09)
    -> ExpYear           -- ^ Required, Expiration Year (i.e. 2018)
    -> CVC               -- ^ Required, CVC Number (i.e. 000)
    -> Currency          -- ^ Required, 3-letter ISO Code
    -> Amount            -- ^ Required, Integer value of 100 represents $1
    -> Maybe Description -- ^ Optional, default is null
    -> Stripe Charge
chargeCard cardNumber expMonth expYear cvc currency amount description =
    chargeBase amount currency description
    Nothing Nothing Nothing Nothing True
    (Just cardNumber) (Just expMonth)
    (Just expYear) (Just cvc) []

------------------------------------------------------------------------------
-- | Base method for creating a `Charge`
chargeBase
    :: Amount             -- ^ Required, Integer value of 100 represents $1
    -> Currency           -- ^ Required, 3-letter ISO Code
    -> Maybe Description  -- ^ Optional, default is null
    -> Maybe CustomerId   -- ^ Optional, either `CustomerId` or `TokenId` has to be specified
    -> Maybe TokenId      -- ^ Optional, either `CustomerId` or `TokenId` has to be specified
    -> Maybe StatementDescription -- ^ Optional, Arbitrary string to include on CC statements
    -> Maybe Email        -- ^ Optional, Arbitrary string to include on CC statements
    -> Capture            -- ^ Optional, default is True
    -> Maybe CardNumber   -- ^ Optional, Credit Card Number
    -> Maybe ExpMonth     -- ^ `Card` Expiration Month
    -> Maybe ExpYear      -- ^ `Card` Expiration Year
    -> Maybe CVC          -- ^ `Card` `CVC`
    -> MetaData           -- ^ `Card` `MetaData`
    -> Stripe Charge
chargeBase
    amount
    currency
    description
    customerid
    tokenId
    statementDescription
    receiptEmail
    capture
    cardNumber
    expMonth
    expYear
    cvc'
    metadata    = callAPI request
  where request = StripeRequest POST url params
        url     = "charges"
        params  = toMetaData metadata ++ getParams [
                     ("amount", toText `fmap` Just amount)
                   , ("customer", (\(CustomerId cid) -> cid) `fmap` customerid)
                   , ("currency", toTextLower `fmap` Just currency)
                   , ("card", (\(TokenId tokenid) -> tokenid) `fmap` tokenId)
                   , ("description", description)
                   , ("statement_description", (\(StatementDescription x) -> x) `fmap` statementDescription)
                   , ("receipt_email", (\(Email email) -> email) `fmap` receiptEmail)
                   , ("capture", (\x -> if x then "true" else "false") `fmap` Just capture)
                   , ("card[number]", (\(CardNumber c) -> c) `fmap` cardNumber)
                   , ("card[exp_month]", (\(ExpMonth m) ->  toText m) `fmap` expMonth)
                   , ("card[exp_year]", (\(ExpYear y) -> toText y) `fmap` expYear)
                   , ("card[cvc]", (\(CVC c) -> c) `fmap` cvc')
                  ]

------------------------------------------------------------------------------
-- | Retrieve a `Charge` by `ChargeId`
getCharge
    :: ChargeId -- ^ The `Charge` to retrive
    -> Stripe Charge
getCharge chargeid = getChargeExpandable chargeid []

------------------------------------------------------------------------------
-- | Retrieve a `Charge` by `ChargeId` with `ExpandParams`
getChargeExpandable
    :: ChargeId     -- ^ The `Charge` retrive
    -> ExpandParams -- ^ The `ExpandParams` to retrive
    -> Stripe Charge
getChargeExpandable
    chargeid
    expandParams = callAPI request
  where request = StripeRequest GET url params
        url     = "charges" </> getChargeId chargeid
        params  = toExpandable expandParams

------------------------------------------------------------------------------
-- | Retrieve all `Charge`s
getCharges
    :: Limit                    -- ^ Defaults to 10 if `Nothing` specified
    -> StartingAfter ChargeId   -- ^ Paginate starting after the following CustomerID
    -> EndingBefore ChargeId    -- ^ Paginate ending before the following CustomerID
    -> Stripe (StripeList Charge)
getCharges
    limit
    startingAfter
    endingBefore =
      getChargesExpandable limit startingAfter endingBefore []

------------------------------------------------------------------------------
-- | Retrieve all `Charge`s
getChargesExpandable
    :: Limit                    -- ^ Defaults to 10 if `Nothing` specified
    -> StartingAfter ChargeId   -- ^ Paginate starting after the following `CustomerId`
    -> EndingBefore ChargeId    -- ^ Paginate ending before the following `CustomerId`
    -> ExpandParams             -- ^ Get Charges with `ExpandParams`
    -> Stripe (StripeList Charge)
getChargesExpandable
    limit
    startingAfter
    endingBefore
    expandParams = callAPI request
  where request = StripeRequest GET url params
        url     = "charges"
        params  = getParams [
            ("limit", toText `fmap` limit )
          , ("starting_after", (\(ChargeId x) -> x) `fmap` startingAfter)
          , ("ending_before", (\(ChargeId x) -> x) `fmap` endingBefore)
          ] ++ toExpandable expandParams


------------------------------------------------------------------------------
-- | Retrieve all `Charge`s for a specified `Customer`
getCustomerCharges
    :: CustomerId
    -> Limit                    -- ^ Defaults to 10 if `Nothing` specified
    -> StartingAfter ChargeId   -- ^ Paginate starting after the following `CustomerId`
    -> EndingBefore ChargeId    -- ^ Paginate ending before the following `CustomerId`
    -> Stripe (StripeList Charge)
getCustomerCharges
    customerid
    limit
    startingAfter
    endingBefore =
      getCustomerChargesExpandable customerid limit
        startingAfter endingBefore []

------------------------------------------------------------------------------
-- | Retrieve all `Charge`s for a specified `Customer` with `ExpandParams`
getCustomerChargesExpandable
    :: CustomerId
    -> Limit                    -- ^ Defaults to 10 if `Nothing` specified
    -> StartingAfter ChargeId   -- ^ Paginate starting after the following `CustomerId`
    -> EndingBefore ChargeId    -- ^ Paginate ending before the following `CustomerId`
    -> ExpandParams             -- ^ Get `Customer` `Charge`s with `ExpandParams`
    -> Stripe (StripeList Charge)
getCustomerChargesExpandable
    customerid
    limit
    startingAfter
    endingBefore
    expandParams = callAPI request
  where request = StripeRequest GET url params
        url     = "charges"
        params  = getParams [
            ("customer", Just $ getCustomerId customerid )
          , ("limit", toText `fmap` limit )
          , ("starting_after", (\(ChargeId x) -> x) `fmap` startingAfter)
          , ("ending_before", (\(ChargeId x) -> x) `fmap` endingBefore)
          ] ++ toExpandable expandParams

------------------------------------------------------------------------------
-- | A `Charge` to be updated
updateCharge
    :: ChargeId    -- ^ The `Charge` to update
    -> Description -- ^ The `Charge` `Description` to update
    -> MetaData    -- ^ The `Charge` `MetaData` to update
    -> Stripe Charge
updateCharge
    chargeid
    description
    metadata    = callAPI request
  where request = StripeRequest POST url params
        url     = "charges" </> getChargeId chargeid
        params  = toMetaData metadata ++ getParams [
                   ("description", Just description)
                  ]

------------------------------------------------------------------------------
-- | a `Charge` to be captured
captureCharge
    :: ChargeId     -- ^ The `ChargeId` of the `Charge` to capture
    -> Maybe Amount -- ^ If Nothing the entire charge will be captured, otherwise the remaining will be refunded
    -> Maybe Email  -- ^ `Email` to send `Charge` receipt
    -> Stripe Charge
captureCharge
    chargeid
    amount
    receiptEmail = callAPI request
  where request  = StripeRequest POST url params
        url      = "charges" </> getChargeId chargeid </> "capture"
        params   = getParams [
                     ("amount", toText `fmap` amount)
                   , ("receipt_email", (\(Email email) -> email) `fmap` receiptEmail)
                   ]
