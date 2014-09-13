{-# LANGUAGE OverloadedStrings #-}
module Web.Stripe.Charge 
    ( -- * Types
      Charge   (..)
    , ChargeId (..)  
    , Currency (..)  
    , StripeList (..)  
      -- * API calls
      ---- * Create Charges
    , chargeCustomer
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
    ) where

import           Control.Applicative
import           Data.Bool
import           Data.Monoid
import           Data.Text                       (Text)
import qualified Data.Text.Encoding              as T
import           Network.Http.Client
import           Web.Stripe.Client.Internal
import           Web.Stripe.Types

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
    Nothing Nothing Nothing Nothing 

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
    Nothing Nothing True Nothing Nothing Nothing Nothing

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
    (Just expYear) (Just cvc)
 
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
    cvc = callAPI request
  where request = StripeRequest POST url params
        url     = "charges"
        params  = getParams [ 
                     ("amount", toText <$> Just amount)
                   , ("customer", (\(CustomerId cid) -> cid) <$> customerId)
                   , ("currency", Just currency)
                   , ("description", description)
                   , ("receipt_email", (\(ReceiptEmail email) -> email) <$> receiptEmail)
                   , ("capture", bool "false" "true" <$> Just capture)
                   , ("card[number]", (\(CardNumber c) -> toText c) <$> cardNumber)
                   , ("card[exp_month]", (\(ExpMonth m) -> toText m) <$> expMonth)
                   , ("card[exp_year]", (\(ExpYear y) -> toText y) <$> expYear)
                   , ("card[cvc]", (\(CVC cvc) -> toText cvc) <$> cvc)
                  ]

------------------------------------------------------------------------------
-- | Retrieve a 'Charge' by 'ChargeId'
getCharge
    :: ChargeId -- ^ The 'Charge' to retrive
    -> Stripe Charge
getCharge (ChargeId charge) = callAPI request
  where request = StripeRequest GET url params
        url     = "charges" </> charge
        params  = []

------------------------------------------------------------------------------
-- | Retrieve all 'Charge's
getCharges :: Stripe (StripeList Charge)
getCharges = callAPI request
  where request = StripeRequest GET url params
        url     = "charges"
        params  = []

------------------------------------------------------------------------------
-- | Retrieve all 'Charge's for a specified 'Customer'
getCustomerCharges
    :: CustomerId
    -> Stripe (StripeList Charge)
getCustomerCharges 
    (CustomerId customerId) = callAPI request
  where request = StripeRequest GET url params
        url     = "charges"
        params  = getParams [ 
                   ("customer", Just customerId )
                  ]


------------------------------------------------------------------------------
-- | A 'Charge' to be updated
updateCharge
    :: ChargeId    -- ^ The Charge to update
    -> Description -- ^ The Charge Description to update
    -> Stripe Charge
updateCharge 
    (ChargeId chargeId)
    description = callAPI request
  where request = StripeRequest POST url params
        url     = "charges" </> chargeId
        params  = getParams [
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
                     ("amount", toText <$> amount)
                   , ("receipt_email", (\(ReceiptEmail email) -> email) <$> receiptEmail)
                   ]
