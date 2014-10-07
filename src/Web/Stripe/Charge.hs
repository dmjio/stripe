{-# LANGUAGE OverloadedStrings #-}
module Web.Stripe.Charge
    ( -- * API
      ---- * Create Charges
      chargeCustomer
    , createCharge
    , chargeCardByToken
      ---- * Get Charge(s)
    , getCharge
    , getCharges
    , getCustomerCharges
      ---- * Update Charge
    , updateCharge
      ---- * Capture Charge
    , captureCharge
      -- * Types
    , Charge       (..)
    , TokenId      (..)
    , ChargeId     (..)
    , CustomerId   (..)
    , Currency     (..)
    , CardNumber   (..)
    , CVC          (..)
    , ExpMonth     (..)
    , ExpYear      (..)
    , Description  (..)
    , StripeList   (..)
    , ReceiptEmail (..)
    , StatementDescription
    , Amount
    , Capture
    ) where

import           Web.Stripe.Client.Internal (Method (GET, POST), Stripe,
                                             StripeRequest (..), callAPI,
                                             getParams, toMetaData, toText,
                                             (</>))

import           Web.Stripe.Types           (Amount, CVC (..), Capture,
                                             CardNumber (..), Charge (..),
                                             ChargeId (..), Currency (..),
                                             CustomerId (..), Description (..),
                                             EndingBefore, ExpMonth (..),
                                             ExpYear (..), Limit, MetaData,
                                             ReceiptEmail (..), StartingAfter,
                                             StatementDescription,
                                             StripeList (..), TokenId (..))

------------------------------------------------------------------------------
-- | Charge 'Customer''s by 'CustomerId'
chargeCustomer
    :: CustomerId   -- ^ The ID of the customer to be charged
    -> Currency     -- ^ Required, 3-letter ISO Code
    -> Amount       -- ^ Required, Integer value of 100 represents $1
    -> Maybe Description -- ^ Optional, default is null
    -> Stripe Charge
chargeCustomer customerId currency amount description =
    createCharge amount currency description (Just customerId)
    Nothing Nothing Nothing False
    Nothing Nothing Nothing Nothing []

------------------------------------------------------------------------------
-- | Charge a card by a 'Token'
chargeCardByToken
    :: TokenId    -- ^ The Token representative of a credit card
    -> Currency   -- ^ Required, 3-letter ISO Code
    -> Amount     -- ^ Required, Integer value of 100 represents $1
    -> Maybe Description -- ^ Optional, default is null
    -> Stripe Charge
chargeCardByToken tokenId currency amount description =
    createCharge amount currency description Nothing (Just tokenId)
    Nothing Nothing True Nothing Nothing Nothing Nothing []

------------------------------------------------------------------------------
-- | Charge a card by 'CardNumber'
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
    createCharge amount currency description
    Nothing Nothing Nothing Nothing True
    (Just cardNumber) (Just expMonth)
    (Just expYear) (Just cvc) []

------------------------------------------------------------------------------
-- | Base method for creating a 'Charge'
createCharge
    :: Amount             -- ^ Required, Integer value of 100 represents $1
    -> Currency           -- ^ Required, 3-letter ISO Code
    -> Maybe Description  -- ^ Optional, default is nullo
    -> Maybe CustomerId   -- ^ Optional, either CustomerId or TokenId has to be specified
    -> Maybe TokenId      -- ^ Optional, either CustomerId or TokenId has to be specified
    -> Maybe StatementDescription -- ^ Optional, Arbitrary string to include on CC statements
    -> Maybe ReceiptEmail -- ^ Optional, Arbitrary string to include on CC statements
    -> Capture            -- ^ Optional, default is True
    -> Maybe CardNumber
    -> Maybe ExpMonth
    -> Maybe ExpYear
    -> Maybe CVC
    -> MetaData
    -> Stripe Charge
createCharge
    amount
    (Currency currency)
    description
    customerId
    tokenId
    statementDescription
    receiptEmail
    capture
    cardNumber
    expMonth
    expYear
    cvc
    metadata    = callAPI request
  where request = StripeRequest POST url params
        url     = "charges"
        params  = toMetaData metadata ++ getParams [
                     ("amount", toText `fmap` Just amount)
                   , ("customer", (\(CustomerId cid) -> cid) `fmap` customerId)
                   , ("currency", Just currency)
                   , ("description", description)
                   , ("receipt_email", (\(ReceiptEmail email) -> email) `fmap` receiptEmail)
                   , ("capture", (\x -> if x then "true" else "false") `fmap` Just capture)
                   , ("card[number]", (\(CardNumber c) -> toText c) `fmap` cardNumber)
                   , ("card[exp_month]", (\(ExpMonth m) -> toText m) `fmap` expMonth)
                   , ("card[exp_year]", (\(ExpYear y) -> toText y) `fmap` expYear)
                   , ("card[cvc]", (\(CVC cvc) -> toText cvc) `fmap` cvc)
                  ]

------------------------------------------------------------------------------
-- | Retrieve a 'Charge' by 'ChargeId'
getCharge
    :: ChargeId -- ^ The 'Charge' to retrive
    -> Stripe Charge
getCharge (ChargeId charge) = callAPI request
  where request = StripeRequest GET url params
        url     = "charges" </> charge
        params  = [ ]

------------------------------------------------------------------------------
-- | Retrieve all 'Charge's
getCharges
    :: Limit                    -- ^ Defaults to 10 if `Nothing` specified
    -> StartingAfter ChargeId   -- ^ Paginate starting after the following CustomerID
    -> EndingBefore ChargeId    -- ^ Paginate ending before the following CustomerID
    -> Stripe (StripeList Charge)
getCharges
    limit
    startingAfter
    endingBefore = callAPI request
  where request = StripeRequest GET url params
        url     = "charges"
        params  = getParams [
            ("limit", toText `fmap` limit )
          , ("starting_after", (\(ChargeId x) -> x) `fmap` startingAfter)
          , ("ending_before", (\(ChargeId x) -> x) `fmap` endingBefore)
          ]

------------------------------------------------------------------------------
-- | Retrieve all 'Charge's for a specified 'Customer'
getCustomerCharges
    :: CustomerId
    -> Limit                    -- ^ Defaults to 10 if `Nothing` specified
    -> StartingAfter ChargeId   -- ^ Paginate starting after the following CustomerID
    -> EndingBefore ChargeId    -- ^ Paginate ending before the following CustomerID
    -> Stripe (StripeList Charge)
getCustomerCharges
    (CustomerId customerId)
    limit
    startingAfter
    endingBefore = callAPI request
  where request = StripeRequest GET url params
        url     = "charges"
        params  = getParams [
            ("customer", Just customerId )
          , ("limit", toText `fmap` limit )
          , ("starting_after", (\(ChargeId x) -> x) `fmap` startingAfter)
          , ("ending_before", (\(ChargeId x) -> x) `fmap` endingBefore)
          ]

------------------------------------------------------------------------------
-- | A 'Charge' to be updated
updateCharge
    :: ChargeId    -- ^ The `Charge` to update
    -> Description -- ^ The `Charge` Description to update
    -> MetaData    -- ^ The `Charge` Description to update
    -> Stripe Charge
updateCharge
    (ChargeId chargeId)
    description
    metadata    = callAPI request
  where request = StripeRequest POST url params
        url     = "charges" </> chargeId
        params  = toMetaData metadata ++ getParams [
                   ("description", Just description)
                  ]

------------------------------------------------------------------------------
-- | a 'Charge' to be captured
captureCharge
    :: ChargeId           -- ^ The Charge to capture
    -> Maybe Amount       -- ^ If Nothing the entire charge will be captured, otherwise the remaining will be refunded
    -> Maybe ReceiptEmail -- ^ Email address to send this charge's receipt to
    -> Stripe Charge
captureCharge
    (ChargeId chargeId)
    amount
    receiptEmail = callAPI request
  where request  = StripeRequest POST url params
        url      = "charges" </> chargeId </> "capture"
        params   = getParams [
                     ("amount", toText `fmap` amount)
                   , ("receipt_email", (\(ReceiptEmail email) -> email) `fmap` receiptEmail)
                   ]
