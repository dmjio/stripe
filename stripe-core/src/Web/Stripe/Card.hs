{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
-------------------------------------------
-- |
-- Module      : Web.Stripe.Card
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- < https:/\/\stripe.com/docs/api#cards >
--
-- @
-- import Web.Stripe
-- import Web.Stripe.Customer
-- import Web.Stripe.Card
--
-- main :: IO ()
-- main = do
--   let config = SecretKey "secret_key"
--       credit = CardNumber "4242424242424242"
--       em  = ExpMonth 12
--       ey  = ExpYear 2015
--       cvc = CVC "123"
--   result <- stripe config $ do
--          Customer { customerId = cid } <- createEmptyCustomer
--          card <- createCustomerCard cid credit em ey cvc
--          return card
--   case result of
--     Right card -> print card
--     Left  stripeError -> print stripeError
-- @
module Web.Stripe.Card
    ( -- * API
      -- ** Customers
      -- *** Create Customer Card
      CreateCustomerCardByToken
    , createCustomerCardByToken
    , CreateRecipientCardByToken
    , createRecipientCardByToken
    , CreateCustomerCard
    , createCustomerCard
    , CreateRecipientCard
    , createRecipientCard

      -- *** Get Customer Card(s)
    , GetCustomerCard
    , getCustomerCard
    , GetRecipientCard
    , getRecipientCard
    , GetCustomerCards
    , getCustomerCards
    , GetRecipientCards
    , getRecipientCards
      -- *** Update Customer Card
    , UpdateCustomerCard
    , updateCustomerCard
    , UpdateRecipientCard
    , updateRecipientCard
      -- *** Delete Card
    , DeleteCustomerCard
    , deleteCustomerCard
    , DeleteRecipientCard
    , deleteRecipientCard
      -- * Types
    , AddressLine1    (..)
    , AddressLine2    (..)
    , AddressCity     (..)
    , AddressCountry  (..)
    , AddressState    (..)
    , AddressZip      (..)
    , Brand           (..)
    , Card            (..)
    , CardId          (..)
    , CardNumber      (..)
    , CVC             (..)
    , EndingBefore    (..)
    , ExpandParams    (..)
    , ExpMonth        (..)
    , ExpYear         (..)
    , Limit           (..)
    , Name            (..)
    , RecipientCard   (..)
    , RecipientCardId (..)
    , RecipientId     (..)
    , StartingAfter   (..)
    ) where

import           Data.Text                (Text)
import           Web.Stripe.StripeRequest (Method (GET, POST, DELETE),
                                           StripeHasParam, StripeRequest (..),
                                           StripeReturn, ToStripeParam(..),
                                           mkStripeRequest)
import           Web.Stripe.Util          ((</>))
import           Web.Stripe.Types         (AddressLine1(..), AddressLine2(..)
                                          , AddressCity(..), AddressCountry(..)
                                          , AddressState(..), AddressZip(..)
                                          , Brand(..), Card(..), CardId(..)
                                          , CardNumber(..), CustomerId(..)
                                          , CVC(..), EndingBefore(..)
                                          , ExpandParams(..)
                                          , ExpMonth(..), ExpYear(..), ID
                                          , Limit(..), Name(..), NewCard(..)
                                          , RecipientCard(..)
                                          , RecipientId(..), RecipientCardId(..)
                                          , StartingAfter(..)
                                          , StripeDeleteResult(..)
                                          , StripeList(..), TokenId(..), URL)
import           Web.Stripe.Types.Util    (getCustomerId, getRecipientId)

------------------------------------------------------------------------------
-- | `Card` creation from a `TokenId`
createCardByToken
    :: URL
    -> ID
    -> TokenId
    -> StripeRequest a
createCardByToken
  prefix
  id_
  tokenid       = request
  where request = mkStripeRequest POST url params
        url     = prefix </> id_ </> "cards"
        params  = toStripeParam tokenid []

data CreateCustomerCardByToken
type instance StripeReturn CreateCustomerCardByToken = Card
createCustomerCardByToken
    :: CustomerId
    -> TokenId
    -> StripeRequest CreateCustomerCardByToken
createCustomerCardByToken
  customerid
  tokenid =
    createCardByToken "customers" (getCustomerId customerid) tokenid

data CreateRecipientCardByToken
type instance StripeReturn CreateRecipientCardByToken = RecipientCard
createRecipientCardByToken
    :: RecipientId
    -> TokenId
    -> StripeRequest CreateRecipientCardByToken
createRecipientCardByToken
  recipientid
  tokenid =
    createCardByToken "recipients" (getRecipientId recipientid) tokenid

------------------------------------------------------------------------------
-- | `Card` creation from card info
data CreateCustomerCard
type instance StripeReturn CreateCustomerCard = Card

createCard
    :: URL
    -> ID
    -> NewCard
    -> StripeRequest a
createCard
  prefix
  id_
  newCard       = request
  where request = mkStripeRequest POST url params
        url     = prefix </> id_ </> "cards"
        params  = toStripeParam newCard []

createCustomerCard
    :: CustomerId
    -> NewCard
    -> StripeRequest CreateCustomerCard
createCustomerCard
  customerid
  newCard = createCard "customers" (getCustomerId customerid) newCard

data CreateRecipientCard
type instance StripeReturn CreateRecipientCard = RecipientCard
createRecipientCard
    :: RecipientId
    -> NewCard
    -> StripeRequest CreateRecipientCard
createRecipientCard
  recipientid
  newCard = createCard "recipients" (getRecipientId recipientid) newCard

------------------------------------------------------------------------------
-- | Get card by `CustomerId` and `CardId`
data GetCustomerCard
type instance StripeReturn GetCustomerCard = Card
instance StripeHasParam GetCustomerCard ExpandParams
getCard
  :: URL
  -> ID
  -> Text -- card id
  -> StripeRequest a
getCard
  prefix
  id_
  cardid_      = request
  where request = mkStripeRequest GET url params
        url     = prefix </> id_ </>
                  "cards" </> cardid_
        params  = []

getCustomerCard
    :: CustomerId -- ^ `CustomerId` of the `Card` to retrieve
    -> CardId     -- ^ `CardId` of the card to retrieve
    -> StripeRequest GetCustomerCard
getCustomerCard
  customerid
  (CardId cardid) = getCard "customers" (getCustomerId customerid) cardid

data GetRecipientCard
type instance StripeReturn GetRecipientCard = RecipientCard
instance StripeHasParam GetRecipientCard ExpandParams
getRecipientCard
    :: RecipientId -- ^ `RecipientId` of the `Card` to retrieve
    -> RecipientCardId     -- ^ `CardId` of the card to retrieve
    -> StripeRequest GetRecipientCard
getRecipientCard
  recipientid
  (RecipientCardId cardid)
    = getCard "recipients" (getRecipientId recipientid) cardid

------------------------------------------------------------------------------
-- | Update a `Card`

updateCard
  :: URL
  -> ID
  -> Text -- ^ cardid
  -> StripeRequest a
updateCard
  prefix
  id_
  cardid_       = request
  where request = mkStripeRequest POST url params
        url     = prefix </> id_ </>
                  "cards" </> cardid_
        params  = []

data UpdateCustomerCard
type instance StripeReturn UpdateCustomerCard = Card
instance StripeHasParam UpdateCustomerCard AddressLine1
instance StripeHasParam UpdateCustomerCard AddressLine2
instance StripeHasParam UpdateCustomerCard AddressCity
instance StripeHasParam UpdateCustomerCard AddressZip
instance StripeHasParam UpdateCustomerCard AddressState
instance StripeHasParam UpdateCustomerCard AddressCountry
instance StripeHasParam UpdateCustomerCard ExpMonth
instance StripeHasParam UpdateCustomerCard ExpYear
instance StripeHasParam UpdateCustomerCard Name
updateCustomerCard
    :: CustomerId
    -> CardId
    -> StripeRequest UpdateCustomerCard
updateCustomerCard
  customerid
  (CardId cardid)
    = updateCard "customers" (getCustomerId customerid) cardid

data UpdateRecipientCard
type instance StripeReturn UpdateRecipientCard = RecipientCard
instance StripeHasParam UpdateRecipientCard AddressLine1
instance StripeHasParam UpdateRecipientCard AddressLine2
instance StripeHasParam UpdateRecipientCard AddressCity
instance StripeHasParam UpdateRecipientCard AddressZip
instance StripeHasParam UpdateRecipientCard AddressState
instance StripeHasParam UpdateRecipientCard AddressCountry
instance StripeHasParam UpdateRecipientCard ExpMonth
instance StripeHasParam UpdateRecipientCard ExpYear
instance StripeHasParam UpdateRecipientCard Name

updateRecipientCard
    :: RecipientId
    -> RecipientCardId
    -> StripeRequest UpdateRecipientCard
updateRecipientCard
  recipientid
  (RecipientCardId cardid)
    = updateCard "recipients" (getRecipientId recipientid) cardid

------------------------------------------------------------------------------
-- | Removes a card from a `Customer`
deleteCard
    :: URL
    -> ID
    -> Text     -- ^ `CardId` associated with `Card` to be deleted
    -> StripeRequest a
deleteCard
    prefix
    id_
    cardid_ = request
  where request = mkStripeRequest DELETE url params
        url     = prefix </> id_ </> "cards" </> cardid_
        params  = []

data DeleteCustomerCard
type instance StripeReturn DeleteCustomerCard = StripeDeleteResult
deleteCustomerCard
    :: CustomerId -- ^ `CustomerId` of the `Card` to retrieve
    -> CardId     -- ^ `CardId` associated with `Card` to be deleted
    -> StripeRequest DeleteCustomerCard
deleteCustomerCard
    customerid
    (CardId cardid) = deleteCard "customers" (getCustomerId customerid) cardid

data DeleteRecipientCard
type instance StripeReturn DeleteRecipientCard = StripeDeleteResult
deleteRecipientCard
    :: RecipientId     -- ^ `RecipientId` of the `Card` to retrieve
    -> RecipientCardId -- ^ `CardId` associated with `Card` to be deleted
    -> StripeRequest DeleteRecipientCard
deleteRecipientCard
    recipientid
    (RecipientCardId cardid)
      = deleteCard "recipients" (getRecipientId recipientid) cardid

------------------------------------------------------------------------------
-- | Retrieve all cards associated with a `Customer`
getCards
    :: URL
    -> ID
    -> StripeRequest a
getCards
    prefix
    id_
    = request
  where request = mkStripeRequest GET url params
        url     = prefix </> id_ </> "cards"
        params  = []

data GetCustomerCards
type instance StripeReturn GetCustomerCards = (StripeList Card)
instance StripeHasParam GetCustomerCards ExpandParams
instance StripeHasParam GetCustomerCards (EndingBefore CardId)
instance StripeHasParam GetCustomerCards Limit
instance StripeHasParam GetCustomerCards (StartingAfter CardId)
getCustomerCards
    :: CustomerId    -- ^ The `CustomerId` associated with the cards
    -> StripeRequest GetCustomerCards
getCustomerCards
    customerid
    = getCards "customers" (getCustomerId customerid)

data GetRecipientCards
type instance StripeReturn GetRecipientCards = (StripeList RecipientCard)
instance StripeHasParam GetRecipientCards ExpandParams
instance StripeHasParam GetRecipientCards (EndingBefore CardId)
instance StripeHasParam GetRecipientCards Limit
instance StripeHasParam GetRecipientCards (StartingAfter CardId)
getRecipientCards
    :: RecipientId    -- ^ The `RecipientId` associated with the cards
    -> StripeRequest GetRecipientCards
getRecipientCards
    recipientid
    = getCards "recipients" (getRecipientId recipientid)
