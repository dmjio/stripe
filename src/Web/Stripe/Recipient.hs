{-# LANGUAGE OverloadedStrings #-}
-------------------------------------------
-- |
-- Module      : Web.Stripe.Recipient
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- < https:/\/\stripe.com/docs/api#recipients >
--
-- @
-- import Web.Stripe         
-- import Web.Stripe.Recipient
--
-- main :: IO ()
-- main = do
--   let config = SecretKey "secret_key"
--   result <- stripe config $ 
--       createRecipient (FirstName "simon")
--                       (LastName "marlow")
--                       Nothing -- What is Simon Marlow's middle initial?
--                       (Invidiual :: RecipientType)
--   case result of
--     Right recipient  -> print recipient
--     Left stripeError -> print stripeError
-- @
module Web.Stripe.Recipient
    ( -- * API
      createRecipient
    , createRecipientByCard
    , createRecipientByToken
    , createRecipientByBank
    , createRecipientBase
    , getRecipient
    , getRecipientExpandable
    , getRecipients
    , getRecipientsExpandable
    , updateRecipientName
    , updateRecipientTaxID
    , updateRecipientBankAccount
    , updateRecipientTokenID
    , updateRecipientDefaultCard
    , updateRecipientEmail
    , updateRecipientDescription
    , updateRecipientMetaData
    , updateRecipientBase
    , deleteRecipient
      -- * Types
    , Recipient     (..)
    , RecipientId   (..)
    , FirstName     (..)
    , LastName      (..)
    , MiddleInitial
    , RecipientType (..)
    , TaxID
    , BankAccount   (..)
    , TokenId
    , CardNumber
    , ExpMonth
    , Email (..)
    , ExpYear
    , CVC
    , Description
    , Limit
    , StripeDeleteResult (..)
    , RoutingNumber  (..)
    , AccountNumber  (..)
    , Country        (..)
    , AddressCity    (..)
    , AddressCountry (..)
    , AddressLine1   (..)
    , AddressLine2   (..)
    , AddressState   (..)
    , AddressZip     (..)
    , BankAccountId  (..)
    , BankAccountStatus (..)
    ) where

import           Data.Monoid                ((<>))
import qualified Data.Text                  as T

import           Web.Stripe.Client.Internal (callAPI, Method(POST,GET,DELETE), Stripe,
                                             StripeRequest(..), toMetaData, getParams,
                                             toText, toExpandable, (</>))
import           Web.Stripe.Types           (AccountNumber (..),
                                             BankAccount (..), CVC, CVC (..), 
                                             CardId (..), CardNumber, BankAccountId (..), BankAccountStatus(..),
                                             CardNumber (..), Country (..),
                                             Description, Email, Email (..),
                                             ExpMonth, ExpMonth (..), ExpYear,
                                             RoutingNumber  (..), AccountNumber  (..),
                                             Country        (..), AddressCity    (..),
                                             AddressCountry (..), AddressLine1   (..),
                                             AddressLine2   (..), AddressState   (..),
                                             AddressZip     (..), ExpYear (..),
                                             FirstName (..), LastName (..), Limit,
                                             MiddleInitial, Recipient (..),
                                             RecipientId (..), ExpandParams,
                                             RecipientType (..), StripeDeleteResult(..),
                                             RoutingNumber (..), EndingBefore, StartingAfter,
                                             StripeList (..), TaxID, TokenId,
                                             TokenId (..), MetaData)
import           Web.Stripe.Types.Util      (getRecipientId)

------------------------------------------------------------------------------
-- | Base Request for issues create `Recipient` requests
createRecipientBase
    :: FirstName           -- ^ First Name of `Recipient`
    -> LastName            -- ^ Last Name of `Recipient`
    -> Maybe MiddleInitial -- ^ Middle Initial of `Recipient`
    -> RecipientType       -- ^ `Individual` or `Corporation`
    -> Maybe TaxID         -- ^ SSN for `Individual`, EIN for `Corporation`
    -> Maybe Country       -- ^ `Country` of BankAccount to attach to `Recipient`
    -> Maybe RoutingNumber -- ^ `RoutingNumber` of BankAccount to attach to `Recipient`
    -> Maybe AccountNumber -- ^ `AccountNumber` of BankAccount to attach to `Recipient`
    -> Maybe TokenId       -- ^ `TokenId` of `Card` or `BankAccount` to attach to a `Recipient`
    -> Maybe CardNumber    -- ^ `CardNumber` to attach to `Card` of `Recipient`
    -> Maybe ExpMonth      -- ^ Expiration Month of `Card`
    -> Maybe ExpYear       -- ^ Expiration Year of `Card`
    -> Maybe CVC           -- ^ CVC of Card
    -> Maybe Email         -- ^ Create `Email` with `Recipient`
    -> Maybe Description   -- ^ Create `Description` with `Recipient`
    -> MetaData            -- ^ The `MetaData` associated with the `Recipient`
    -> Stripe Recipient
createRecipientBase
    (FirstName firstName)
    (LastName lastName)
    middleInitial
    recipienttype
    taxId
    country
    routingNumber
    accountNumber
    tokenId
    cardNumber
    expMonth
    expYear
    cvc
    email
    description
    metadata    = callAPI request
  where request = StripeRequest POST url params
        url     = "recipients"
        params  =
            let name   = firstName <> middle <> lastName
                middle = maybe " " (\x -> " " <> T.singleton x <> " ") middleInitial
            in toMetaData metadata ++ getParams [
                    ("name", Just name)
                  , ("type", toText `fmap` Just recipienttype)
                  , ("tax_id", taxId)
                  , ("bank_account[country]", (\(Country x) -> x) `fmap` country)
                  , ("bank_account[routing_number]",  (\(RoutingNumber x ) -> x) `fmap` routingNumber)
                  , ("bank_account[account_number]",  (\(AccountNumber x ) -> x) `fmap` accountNumber)
                  , ("card", (\(TokenId x) -> x) `fmap` tokenId)
                  , ("card[number]", (\(CardNumber x) -> x) `fmap` cardNumber)
                  , ("card[exp_month]", (\(ExpMonth x) -> toText x) `fmap` expMonth)
                  , ("card[exp_year]", (\(ExpYear x) -> toText x) `fmap` expYear)
                  , ("card[cvc]", (\(CVC x) -> x) `fmap` cvc)
                  , ("email", (\(Email x) -> x) `fmap` email)
                  , ("description", description)
                  ]
------------------------------------------------------------------------------
-- | Create a `Recipient`
createRecipient
    :: FirstName           -- ^ First Name of 'Recipient'
    -> LastName            -- ^ Last Name of 'Recipient'
    -> Maybe MiddleInitial -- ^ Middle Initial of 'Recipient'
    -> RecipientType       -- ^ 'Individual' or 'Corporation'
    -> Stripe Recipient
createRecipient
    firstName
    lastName
    middleInitial
    recipienttype
    = createRecipientBase firstName lastName middleInitial recipienttype
      Nothing Nothing Nothing Nothing Nothing Nothing Nothing
      Nothing Nothing Nothing Nothing []
------------------------------------------------------------------------------
-- | Create a `Recipient` by a `Card`
createRecipientByCard
    :: FirstName           -- ^ First Name of 'Recipient'
    -> LastName            -- ^ Last Name of 'Recipient'
    -> Maybe MiddleInitial -- ^ Middle Initial of 'Recipient'
    -> RecipientType       -- ^ 'Individual' or 'Corporation'
    -> CardNumber          -- ^ 'Card' Number
    -> ExpMonth            -- ^ Expiration Month
    -> ExpYear             -- ^ Expiration Year
    -> CVC                 -- ^ 'CVC' (i.e. 117)
    -> Stripe Recipient
createRecipientByCard
    firstName
    lastName
    middleInitial
    recipienttype
    cardNumber
    expMonth
    expYear
    cvc
    = createRecipientBase firstName lastName middleInitial recipienttype
      Nothing Nothing Nothing Nothing Nothing (Just cardNumber) (Just expMonth) 
      (Just expYear) (Just cvc) Nothing Nothing []

------------------------------------------------------------------------------
-- | Create a `Recipient` by specifying a `TokenId`
createRecipientByToken
    :: FirstName           -- ^ First Name of `Recipient`
    -> LastName            -- ^ Last Name of `Recipient`
    -> Maybe MiddleInitial -- ^ Middle Initial of `Recipient`
    -> RecipientType       -- ^ `Individual` or `Corporation`
    -> TokenId             -- ^ `TokenId` received from stripe.js or Token API
    -> Stripe Recipient
createRecipientByToken
    firstName
    lastName
    middleInitial
    recipienttype
    tokenId
    = createRecipientBase firstName lastName middleInitial recipienttype
      Nothing Nothing Nothing Nothing (Just tokenId) Nothing Nothing
      Nothing Nothing Nothing Nothing []

------------------------------------------------------------------------------
-- | Create a `Recipient` with a `BankAccount`
createRecipientByBank
    :: FirstName           -- ^ First Name of `Recipient`
    -> LastName            -- ^ Last Name of `Recipient`
    -> Maybe MiddleInitial -- ^ Middle Initial of `Recipient`
    -> RecipientType       -- ^ `Individual` or `Corporation`
    -> Country             -- ^ `Country` of BankAccount to attach to `Recipient`
    -> RoutingNumber       -- ^ `RoutingNumber` of BankAccount to attach to `Recipient`
    -> AccountNumber       -- ^ `AccountNumber` of BankAccount to attach to `Recipient`
    -> Stripe Recipient
createRecipientByBank
    firstName
    lastName
    middleInitial
    recipienttype
    country
    routingNumber
    accountNumber
    = createRecipientBase firstName lastName middleInitial recipienttype
      Nothing (Just country) (Just routingNumber) (Just accountNumber) Nothing Nothing
      Nothing Nothing Nothing Nothing Nothing []

------------------------------------------------------------------------------
-- | Retrieve a 'Recipient'
getRecipient
    :: RecipientId -- ^ The `RecipientId` of the `Recipient` to be retrieved
    -> Stripe Recipient
getRecipient
    recipientid = getRecipientExpandable recipientid []

------------------------------------------------------------------------------
-- | Retrieve a `Recipient`
getRecipientExpandable
    :: RecipientId   -- ^ The `RecipientId` of the `Recipient` to be retrieved
    -> ExpandParams  -- ^ `ExpandParams` of the object to be expanded
    -> Stripe Recipient
getRecipientExpandable
    recipientid
    expandParams = callAPI request
  where request =  StripeRequest GET url params
        url     = "recipients" </> getRecipientId recipientid
        params  = toExpandable expandParams

------------------------------------------------------------------------------
-- | Retrieve multiple 'Recipient's
getRecipients
    :: Limit                     -- ^ Defaults to 10 if `Nothing` specified
    -> StartingAfter RecipientId -- ^ Paginate starting after the following `RecipientId`
    -> EndingBefore RecipientId  -- ^ Paginate ending before the following `RecipientId`
    -> Stripe (StripeList Recipient)
getRecipients
  limit
  startingAfter
  endingBefore  =
    getRecipientsExpandable
      limit startingAfter endingBefore []

------------------------------------------------------------------------------
-- | Retrieve multiple 'Recipient's with `ExpandParams`
getRecipientsExpandable
    :: Limit                     -- ^ Defaults to 10 if `Nothing` specified
    -> StartingAfter RecipientId -- ^ Paginate starting after the following `RecipientId`
    -> EndingBefore RecipientId  -- ^ Paginate ending before the following `RecipientId`
    -> ExpandParams              -- ^ `ExpandParams` of the object to be expanded
    -> Stripe (StripeList Recipient)
getRecipientsExpandable
  limit
  startingAfter
  endingBefore
  expandParams  = callAPI request
  where request =  StripeRequest GET url params
        url     = "recipients"
        params  = getParams [
            ("limit", toText `fmap` limit )
          , ("starting_after", (\(RecipientId x) -> x) `fmap` startingAfter)
          , ("ending_before", (\(RecipientId x) -> x) `fmap` endingBefore)
          ] ++ toExpandable expandParams


------------------------------------------------------------------------------
-- | Base Request for updating a `Recipient`, useful for creating custom `Recipient` update functions
updateRecipientBase
    :: RecipientId         -- ^ The `RecipientId` of the `Recipient` to be updated
    -> Maybe FirstName     -- ^ First Name of `Recipient`
    -> Maybe LastName      -- ^ Last Name of `Recipient`
    -> Maybe MiddleInitial -- ^ Middle Initial of `Recipient`
    -> Maybe TaxID         -- ^ SSN for `Individual`, EIN for `Corporation`
    -> Maybe Country       -- ^ `Country` of BankAccount to attach to `Recipient`
    -> Maybe RoutingNumber -- ^ `RoutingNumber` of BankAccount to attach to `Recipient`
    -> Maybe AccountNumber -- ^ `AccountNumber` of BankAccount to attach to `Recipient`
    -> Maybe TokenId       -- ^ `TokenId` of `Card` to attach to a `Recipient`
    -> Maybe CardNumber    -- ^ `CardNumber` to attach to `Card` of `Recipient`
    -> Maybe ExpMonth      -- ^ Expiration Month of `Card`
    -> Maybe ExpYear       -- ^ Expiration Year of `Card`
    -> Maybe CVC           -- ^ CVC of Card
    -> Maybe CardId        -- ^ The Default `Card` for this `Recipient` to use
    -> Maybe Email         -- ^ Create `Email` with `Recipient`
    -> Maybe Description   -- ^ Create `Description` with `Recipient`
    -> MetaData            -- ^ The `MetaData` associated with the `Recipient`
    -> Stripe Recipient
updateRecipientBase
    recipientid
    firstName
    lastName
    middleInitial
    taxId
    country
    routingNumber
    accountNumber
    tokenId
    cardNumber
    expMonth
    expYear
    cvc
    cardId
    email
    description
    metadata    = callAPI request
  where request = StripeRequest POST url params
        url     = "recipients" </> getRecipientId recipientid
        params  =
            let name = if firstName == Nothing || lastName == Nothing
                         then Nothing
                         else do let Just (FirstName f) = firstName
                                     Just (LastName l)  = lastName
                                     middle = maybe " " (\x -> " " <> T.singleton x <> " ") middleInitial
                                 Just $ f <> middle <> l
            in getParams [
                    ("name", name)
                  , ("tax_id", taxId)
                  , ("bank_account[country]", (\(Country x) -> x) `fmap` country)
                  , ("bank_account[routing_number]",  (\(RoutingNumber x ) -> x) `fmap` routingNumber)
                  , ("bank_account[account_number]",  (\(AccountNumber x ) -> x) `fmap` accountNumber)
                  , ("card", (\(TokenId x) -> x) `fmap` tokenId)
                  , ("card[number]", (\(CardNumber x) -> x) `fmap` cardNumber)
                  , ("card[exp_month]", (\(ExpMonth x) -> toText x) `fmap` expMonth)
                  , ("card[exp_year]", (\(ExpYear x) -> toText x) `fmap` expYear)
                  , ("card[cvc]", (\(CVC x) -> x) `fmap` cvc)
                  , ("default_card", (\(CardId x) -> x) `fmap` cardId)
                  , ("email", (\(Email x) -> x) `fmap` email)
                  , ("description", description)
                  ] ++ toMetaData metadata

------------------------------------------------------------------------------
-- | Update a `Recipient` `FirstName`, `LastName` and/or `MiddleInitial`
updateRecipientName
    :: RecipientId   -- ^ The `RecipientId` of the `Recipient` to be updated
    -> FirstName     -- ^ First Name of `Recipient`
    -> LastName      -- ^ Last Name of `Recipient`
    -> MiddleInitial -- ^ Middle Initial of `Recipient`
    -> Stripe Recipient
updateRecipientName
    recipientid
    firstName
    lastName
    middleInitial = updateRecipientBase
                     recipientid (Just firstName) (Just lastName) (Just middleInitial)
                     Nothing Nothing Nothing Nothing
                     Nothing Nothing Nothing Nothing
                     Nothing Nothing Nothing Nothing []

------------------------------------------------------------------------------
-- | Update a 'Recipient' 'BankAccount'
updateRecipientBankAccount
    :: RecipientId   -- ^ The `RecipientId` of the `Recipient` to be updated
    -> Country       -- ^ `Country` of BankAccount to attach to `Recipient`
    -> RoutingNumber -- ^ `RoutingNumber` of BankAccount to attach to `Recipient`
    -> AccountNumber -- ^ `AccountNumber` of BankAccount to attach to `Recipient`
    -> Stripe Recipient
updateRecipientBankAccount
    recipientid
    country
    routingNumber
    accountNumber
     = updateRecipientBase
         recipientid Nothing Nothing Nothing
         Nothing (Just country) (Just routingNumber)
         (Just accountNumber) Nothing Nothing
         Nothing Nothing Nothing Nothing
         Nothing Nothing []

------------------------------------------------------------------------------
-- | Update a `Recipient` `TaxID`
updateRecipientTaxID
    :: RecipientId   -- ^ The `RecipientId` of the `Recipient` to be updated
    -> TaxID         -- ^ `TaxID` of `Recipient` to be updated
    -> Stripe Recipient
updateRecipientTaxID
    recipientid
    taxID = updateRecipientBase
              recipientid Nothing Nothing Nothing
              (Just taxID) Nothing Nothing Nothing
              Nothing Nothing Nothing Nothing
              Nothing Nothing Nothing Nothing []

------------------------------------------------------------------------------
-- | Update a `Recipient` `Card` by `TokenId`
updateRecipientTokenID
    :: RecipientId   -- ^ The `RecipientId` of the `Recipient` to be updated
    -> TokenId       -- ^ `TaxID` of `Recipient` to be updated
    -> Stripe Recipient
updateRecipientTokenID
    recipientid
    tokenId = updateRecipientBase
              recipientid Nothing Nothing Nothing
              Nothing Nothing Nothing Nothing (Just tokenId) Nothing
              Nothing Nothing Nothing Nothing Nothing Nothing []


------------------------------------------------------------------------------
-- | Update default `Card` of `Recipient`
updateRecipientDefaultCard
    :: RecipientId   -- ^ The `RecipientId` of the `Recipient` to be updated
    -> CardId        -- ^ `CardId` of `Card` to be made default
    -> Stripe Recipient
updateRecipientDefaultCard
    recipientid
    cardId = updateRecipientBase
              recipientid Nothing Nothing Nothing Nothing
                          Nothing Nothing Nothing Nothing
                          Nothing Nothing Nothing Nothing
                          (Just cardId) Nothing Nothing []

------------------------------------------------------------------------------
-- | Update a `Recipient` `Email` Address
updateRecipientEmail
    :: RecipientId   -- ^ The `RecipientId` of the `Recipient` to be updated
    -> Email         -- ^ `Email` of `Recipient` to be updated
    -> Stripe Recipient
updateRecipientEmail
    recipientid
    email = updateRecipientBase
              recipientid Nothing Nothing Nothing Nothing
                          Nothing Nothing Nothing Nothing
                          Nothing Nothing Nothing Nothing
                          Nothing (Just email) Nothing []

------------------------------------------------------------------------------
-- | Update a `Recipient` `Description`
updateRecipientDescription
    :: RecipientId   -- ^ The `RecipientId` of the `Recipient` to be updated
    -> Description   -- ^ `Description` of `Recipient` to be updated
    -> Stripe Recipient
updateRecipientDescription
    recipientid
    description = updateRecipientBase
                   recipientid Nothing Nothing Nothing Nothing
                               Nothing Nothing Nothing Nothing
                               Nothing Nothing Nothing Nothing
                               Nothing Nothing (Just description) []

------------------------------------------------------------------------------
-- | Update a `Recipient` `MetaData`
updateRecipientMetaData
    :: RecipientId   -- ^ The `RecipientId` of the `Recipient` to be updated
    -> MetaData      -- ^ The `MetaData` associated with the `Recipient`
    -> Stripe Recipient
updateRecipientMetaData
    recipientid
    metadata = updateRecipientBase
               recipientid Nothing Nothing Nothing Nothing
               Nothing Nothing Nothing Nothing
               Nothing Nothing Nothing Nothing
               Nothing Nothing Nothing metadata

------------------------------------------------------------------------------
-- | Delete a `Recipient`
deleteRecipient
    :: RecipientId   -- ^ `RecipiendId` of `Recipient` to delete
    -> Stripe StripeDeleteResult
deleteRecipient
   recipientid = callAPI request
  where request =  StripeRequest DELETE url params
        url     = "recipients" </> getRecipientId recipientid
        params  = []
