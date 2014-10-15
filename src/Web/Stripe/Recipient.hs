{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Web.Stripe.Recipient
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
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
    , updateRecipientCard
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
    , ExpYear
    , CVC
    , Email
    , Description
    , Limit
    ) where

import           Data.Monoid                ((<>))
import qualified Data.Text                  as T

import           Web.Stripe.Client.Internal
import           Web.Stripe.Types           (AccountNumber (..),
                                             BankAccount (..), CVC, CVC (..),
                                             CardId (..), CardNumber,
                                             CardNumber (..), Country (..),
                                             Description, Email, Email (..),
                                             ExpMonth, ExpMonth (..), ExpYear,
                                             ExpYear (..), FirstName (..),
                                             LastName (..), Limit,
                                             MiddleInitial, Recipient (..),
                                             RecipientId (..), ExpandParams,
                                             RecipientType (..), 
                                             RoutingNumber (..), EndingBefore, StartingAfter,
                                             StripeList (..), TaxID, TokenId,
                                             TokenId (..), MetaData)
import           Web.Stripe.Types.Util

------------------------------------------------------------------------------
-- | Base Request for issues create `Recipient` requests
createRecipientBase
    :: FirstName           -- ^ First Name of `Recipient`
    -> LastName            -- ^ Last Name of `Recipient`
    -> Maybe MiddleInitial -- ^ Middle Initial of `Recipient`
    -> RecipientType       -- ^ `Individual` or `Corporation`
    -> Maybe TaxID         -- ^ SSN for `Individual`, EIN for `Corporation`
    -> Maybe BankAccount   -- ^ `BankAccount` to attach to `Recipient`
    -> Maybe TokenId       -- ^ `TokenId` of `Card` to attach to a `Recipient`
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
    bankAccount
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
                  , ("bank_account[country]", ((\(BankAccount { bankAccountCountry = Country x }) -> x) `fmap` bankAccount))
                  , ("bank_account[routing_number]",  ((\(BankAccount { bankAccountRoutingNumber = RoutingNumber x }) -> x) `fmap` bankAccount))
                  , ("bank_account[account_number]",  ((\(BankAccount { bankAccountNumber = AccountNumber x }) -> x) `fmap` bankAccount))
                  , ("card", (\(TokenId x) -> x) `fmap` tokenId)
                  , ("card[number]", (\(CardNumber x) -> x) `fmap` cardNumber)
                  , ("card[exp_month]", (\(ExpMonth x) -> toText x) `fmap` expMonth)
                  , ("card[exp_year]", (\(ExpYear x) -> toText x) `fmap` expYear)
                  , ("card[cvc]", (\(CVC x) -> x) `fmap` cvc)
                  , ("email", (\(Email x) -> x) `fmap` email)
                  , ("description", description)
                  ]
------------------------------------------------------------------------------
-- | Simplest create 'Recipient' request
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
      Nothing Nothing Nothing Nothing Nothing
      Nothing Nothing Nothing Nothing []
------------------------------------------------------------------------------
-- | Create a 'Recipient' with an attached 'Card'
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
      Nothing Nothing Nothing (Just cardNumber) (Just expMonth)
      (Just expYear) (Just cvc) Nothing Nothing []

------------------------------------------------------------------------------
-- | Create a 'Recipient' by specifying a 'TokenId'
createRecipientByToken
    :: FirstName           -- ^ First Name of 'Recipient'
    -> LastName            -- ^ Last Name of 'Recipient'
    -> Maybe MiddleInitial -- ^ Middle Initial of 'Recipient'
    -> RecipientType       -- ^ 'Individual' or 'Corporation'
    -> TokenId             -- ^ 'TokenId' received from stripe.js or Token API
    -> Stripe Recipient
createRecipientByToken
    firstName
    lastName
    middleInitial
    recipienttype
    tokenId
    = createRecipientBase firstName lastName middleInitial recipienttype
      Nothing Nothing (Just tokenId) Nothing Nothing
      Nothing Nothing Nothing Nothing []

------------------------------------------------------------------------------
-- | Create a 'Recipient' with a 'BankAccount'
createRecipientByBank
    :: FirstName           -- ^ First Name of 'Recipient'
    -> LastName            -- ^ Last Name of 'Recipient'
    -> Maybe MiddleInitial -- ^ Middle Initial of 'Recipient'
    -> RecipientType       -- ^ 'Individual' or 'Corporation'
    -> BankAccount         -- ^ 'Card' Number
    -> Stripe Recipient
createRecipientByBank
    firstName
    lastName
    middleInitial
    recipienttype
    bankAccount
    = createRecipientBase firstName lastName middleInitial recipienttype
      Nothing (Just bankAccount) Nothing Nothing Nothing Nothing Nothing Nothing Nothing []

------------------------------------------------------------------------------
-- | Retrieve a 'Recipient'
getRecipient
    :: RecipientId -- ^ The 'RecipientId' of the 'Recipient' to be retrieved
    -> Stripe Recipient
getRecipient
    recipientid = getRecipientExpandable recipientid []

------------------------------------------------------------------------------
-- | Retrieve a 'Recipient'
getRecipientExpandable
    :: RecipientId -- ^ The 'RecipientId' of the 'Recipient' to be retrieved
    -> ExpandParams
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
    -> ExpandParams
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
-- | Base Request for updating a 'Recipient', useful for creating custom 'Recipient' update functions
updateRecipientBase
    :: RecipientId         -- ^ The 'RecipientId' of the 'Recipient' to be updated
    -> Maybe FirstName     -- ^ First Name of 'Recipient'
    -> Maybe LastName      -- ^ Last Name of 'Recipient'
    -> Maybe MiddleInitial -- ^ Middle Initial of 'Recipient'
    -> Maybe TaxID         -- ^ SSN for 'Individual', EIN for 'Corporation'
    -> Maybe BankAccount   -- ^ 'BankAccount' to attach to 'Recipient'
    -> Maybe TokenId       -- ^ 'TokenId' of 'Card' to attach to a 'Recipient'
    -> Maybe CardNumber    -- ^ 'CardNumber' to attach to 'Card' of 'Recipient'
    -> Maybe ExpMonth      -- ^ Expiration Month of 'Card'
    -> Maybe ExpYear       -- ^ Expiration Year of 'Card'
    -> Maybe CVC           -- ^ CVC of Card
    -> Maybe CardId        -- ^ The Default 'Card' for this 'Recipient' to use
    -> Maybe Email         -- ^ Create 'Email' with 'Recipient'
    -> Maybe Description   -- ^ Create 'Description' with 'Recipient'
    -> MetaData            -- ^ The `MetaData` associated with the `Recipient`
    -> Stripe Recipient
updateRecipientBase
    recipientid
    firstName
    lastName
    middleInitial
    taxId
    bankAccount
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
                  , ("bank_account[country]", ((\(BankAccount { bankAccountCountry = Country x }) -> x) `fmap` bankAccount))
                  , ("bank_account[routing_number]",  ((\(BankAccount { bankAccountRoutingNumber = RoutingNumber x }) -> x) `fmap` bankAccount))
                  , ("bank_account[account_number]",  ((\(BankAccount { bankAccountNumber = AccountNumber x }) -> x) `fmap` bankAccount))
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
-- | Update a 'Recipient' 'FirstName', 'LastName' and/or 'MiddleInitial'
updateRecipientName
    :: RecipientId   -- ^ The 'RecipientId' of the 'Recipient' to be updated
    -> FirstName     -- ^ First Name of 'Recipient'
    -> LastName      -- ^ Last Name of 'Recipient'
    -> MiddleInitial -- ^ Middle Initial of 'Recipient'
    -> Stripe Recipient
updateRecipientName
    recipientid
    firstName
    lastName
    middleInitial = updateRecipientBase
                     recipientid (Just firstName) (Just lastName) (Just middleInitial)
                     Nothing Nothing Nothing Nothing
                     Nothing Nothing Nothing Nothing
                     Nothing Nothing []

------------------------------------------------------------------------------
-- | Update a 'Recipient' 'BankAccount'
--
-- > runStripe config $ updateRecipient (RecipientId "rp_4lpjaLFB5ecSks") BankAccount {
-- >     bankAccountCountry = Country "us"
-- >   , bankAccountRoutingNumber = RoutingNumber "071000013"
-- >   , bankAccountNumber = AccountNumber "293058719045"
-- >  }
--
updateRecipientBankAccount
    :: RecipientId   -- ^ The 'RecipientId' of the 'Recipient' to be updated
    -> BankAccount   -- ^ 'BankAccount' of the 'Recipient' to be updated
    -> Stripe Recipient
updateRecipientBankAccount
    recipientid
    bankAccount = updateRecipientBase
         recipientid Nothing Nothing Nothing
         Nothing (Just bankAccount)Nothing Nothing
         Nothing Nothing Nothing Nothing
         Nothing Nothing []

------------------------------------------------------------------------------
-- | Update a 'Recipient' 'TaxID'
--
-- > runStripe config $ updateRecipientTaxID (RecipientId "rp_4lpjaLFB5ecSks") "SampleTaxID"
--
updateRecipientTaxID
    :: RecipientId   -- ^ The 'RecipientId' of the 'Recipient' to be updated
    -> TaxID         -- ^ 'TaxID' of 'Recipient' to be updated
    -> Stripe Recipient
updateRecipientTaxID
    recipientid
    taxID = updateRecipientBase
              recipientid Nothing Nothing Nothing
              (Just taxID) Nothing Nothing Nothing
              Nothing Nothing Nothing Nothing
              Nothing Nothing []

------------------------------------------------------------------------------
-- | Update a 'Recipient' 'Card' by 'TokenId'
--
-- > runStripe config $ updateRecipientTokenId (RecipientId "rp_4lpjaLFB5ecSks") (TokenId "tok_aksdjfh9823")
--
updateRecipientTokenID
    :: RecipientId   -- ^ The 'RecipientId' of the 'Recipient' to be updated
    -> TokenId       -- ^ 'TaxID' of 'Recipient' to be updated
    -> Stripe Recipient
updateRecipientTokenID
    recipientid
    tokenId = updateRecipientBase
              recipientid Nothing Nothing Nothing
              Nothing Nothing (Just tokenId) Nothing
              Nothing Nothing Nothing Nothing
              Nothing Nothing []

------------------------------------------------------------------------------
-- | Update a 'Recipient' 'Card'
--
-- > runStripe config $ updateRecipientCard (RecipientId "rp_4lpjaLFB5ecSks") number month year cvc
-- >   where
-- >     number = CardNumber "4242424242424242"
-- >     month  = ExpMonth 12
-- >     year   = ExpYear 2018
-- >     cvc    = 117
--
updateRecipientCard
    :: RecipientId -- ^ The 'RecipientId' of the 'Recipient' to be updated
    -> CardNumber  -- ^ 'CardNumber' to attach to 'Card' of 'Recipient'
    -> ExpMonth    -- ^ Expiration Month of 'Card'
    -> ExpYear     -- ^ Expiration Year of 'Card'
    -> CVC         -- ^ CVC of Card
    -> Stripe Recipient
updateRecipientCard
    recipientid
    cardNumber
    expMonth
    expYear
    cvc = updateRecipientBase
              recipientid Nothing Nothing Nothing
              Nothing Nothing Nothing (Just cardNumber)
              (Just expMonth) (Just expYear) (Just cvc)
              Nothing Nothing Nothing []

------------------------------------------------------------------------------
-- | Update default 'Card' of 'Recipient'
--
-- > runStripe config $ updateRecipientDefaultCard (RecipientId "rp_4lpjaLFB5ecSks") (CardId "card_4jQs35jE5wFOor")
--
updateRecipientDefaultCard
    :: RecipientId   -- ^ The 'RecipientId' of the 'Recipient' to be updated
    -> CardId        -- ^ 'CardId' of 'Card' to be made default
    -> Stripe Recipient
updateRecipientDefaultCard
    recipientid
    cardId = updateRecipientBase
              recipientid Nothing Nothing Nothing Nothing
                          Nothing Nothing Nothing Nothing
                          Nothing Nothing (Just cardId) Nothing
                          Nothing []

------------------------------------------------------------------------------
-- | Update a 'Recipient' 'Email' Address
--
-- > runStripe config $ updateRecipientEmail (RecipientId "rp_4lpjaLFB5ecSks") (Email "name@domain.com")
--
updateRecipientEmail
    :: RecipientId   -- ^ The 'RecipientId' of the 'Recipient' to be updated
    -> Email         -- ^ 'Email' of 'Recipient' to be updated
    -> Stripe Recipient
updateRecipientEmail
    recipientid
    email = updateRecipientBase
              recipientid Nothing Nothing Nothing Nothing
                          Nothing Nothing Nothing Nothing
                          Nothing Nothing Nothing (Just email)
                          Nothing []

------------------------------------------------------------------------------
-- | Update a 'Recipient' 'Description'
--
-- > runStripe config $ updateRecipientDescription (RecipientId "rp_4lpjaLFB5ecSks") (Email "name@domain.com")
--
updateRecipientDescription
    :: RecipientId   -- ^ The 'RecipientId' of the 'Recipient' to be updated
    -> Description   -- ^ 'Description' of 'Recipient' to be updated
    -> Stripe Recipient
updateRecipientDescription
    recipientid
    description = updateRecipientBase
                   recipientid Nothing Nothing Nothing Nothing
                               Nothing Nothing Nothing Nothing
                               Nothing Nothing Nothing Nothing
                              (Just description) []

------------------------------------------------------------------------------
-- | Update a `Recipient` `MetaData`
--
-- > runStripe config $ updateRecipientMetaData (RecipientId "rp_4lpjaLFB5ecSks") [("key", "value")]
--
updateRecipientMetaData
    :: RecipientId   -- ^ The 'RecipientId' of the 'Recipient' to be updated
    -> MetaData      -- ^ The `MetaData` associated with the `Recipient`
    -> Stripe Recipient
updateRecipientMetaData
    recipientid
    metadata = updateRecipientBase
               recipientid Nothing Nothing Nothing Nothing
               Nothing Nothing Nothing Nothing
               Nothing Nothing Nothing Nothing
               Nothing metadata

------------------------------------------------------------------------------
-- | Delete a 'Recipient'
--
-- > runStripe config $ deleteRecipient (RecipientId "rp_4lpjaLFB5ecSks")
--
deleteRecipient
    :: RecipientId
    -> Stripe Recipient
deleteRecipient
   recipientid = callAPI request
  where request =  StripeRequest DELETE url params
        url     = "recipients" </> getRecipientId recipientid
        params  = []
