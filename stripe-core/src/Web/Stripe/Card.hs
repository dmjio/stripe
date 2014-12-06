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
      CreateCardByToken
    , createCustomerCardByToken
    , createRecipientCardByToken
    , CreateCard
    , createCustomerCard
    , createRecipientCard
      -- *** Get Customer Card(s)
    , GetCard
    , getCustomerCard
    , getRecipientCard
    , GetCards
    , getCustomerCards
    , getRecipientCards
      -- *** Update Customer Card
    , UpdateCard
    , updateCustomerCard
    , updateRecipientCard
      -- *** Delete Card
    , DeleteCard
    , deleteCustomerCard
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
import           Web.Stripe.Types.Util    (getCardId, getCustomerId,
                                           getRecipientId)

------------------------------------------------------------------------------
-- | `Card` creation from a `TokenId`
data CreateCardByToken
type instance StripeReturn CreateCardByToken = Card
createCardByToken
    :: URL
    -> ID
    -> TokenId
    -> StripeRequest CreateCardByToken
createCardByToken
  prefix
  id_
  tokenid       = request
  where request = mkStripeRequest POST url params
        url     = prefix </> id_ </> "cards"
        params  = toStripeParam tokenid []

createCustomerCardByToken
    :: CustomerId
    -> TokenId
    -> StripeRequest CreateCardByToken
createCustomerCardByToken
  customerid
  tokenid =
    createCardByToken "customers" (getCustomerId customerid) tokenid

createRecipientCardByToken
    :: RecipientId
    -> TokenId
    -> StripeRequest CreateCardByToken
createRecipientCardByToken
  recipientid
  tokenid =
    createCardByToken "recipients" (getRecipientId recipientid) tokenid

------------------------------------------------------------------------------
-- | `Card` creation from card info
data CreateCard
type instance StripeReturn CreateCard = Card

createCard
    :: URL
    -> ID
    -> NewCard
    -> StripeRequest CreateCard
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
    -> StripeRequest CreateCard
createCustomerCard
  customerid
  newCard = createCard "customers" (getCustomerId customerid) newCard

createRecipientCard
    :: RecipientId
    -> NewCard
    -> StripeRequest CreateCard
createRecipientCard
  recipientid
  newCard = createCard "recipients" (getRecipientId recipientid) newCard

------------------------------------------------------------------------------
-- | Get card by `CustomerId` and `CardId`
data GetCard
type instance StripeReturn GetCard = Card
instance StripeHasParam GetCard ExpandParams
getCard
  :: URL
  -> ID
  -> Text -- card id
  -> StripeRequest GetCard
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
    -> StripeRequest GetCard
getCustomerCard
  customerid
  (CardId cardid) = getCard "customers" (getCustomerId customerid) cardid

getRecipientCard
    :: RecipientId -- ^ `RecipientId` of the `Card` to retrieve
    -> RecipientCardId     -- ^ `CardId` of the card to retrieve
    -> StripeRequest GetCard
getRecipientCard
  recipientid
  (RecipientCardId cardid)
    = getCard "recipients" (getRecipientId recipientid) cardid

------------------------------------------------------------------------------
-- | Update a `Card`
data UpdateCard
type instance StripeReturn UpdateCard = Card
instance StripeHasParam UpdateCard AddressLine1
instance StripeHasParam UpdateCard AddressLine2
instance StripeHasParam UpdateCard AddressCity
instance StripeHasParam UpdateCard AddressZip
instance StripeHasParam UpdateCard AddressState
instance StripeHasParam UpdateCard AddressCountry
instance StripeHasParam UpdateCard ExpMonth
instance StripeHasParam UpdateCard ExpYear
instance StripeHasParam UpdateCard Name

updateCard
  :: URL
  -> ID
  -> Text -- ^ cardid
  -> StripeRequest UpdateCard
updateCard
  prefix
  id_
  cardid_       = request
  where request = mkStripeRequest POST url params
        url     = prefix </> id_ </>
                  "cards" </> cardid_
        params  = []

updateCustomerCard
    :: CustomerId
    -> CardId
    -> StripeRequest UpdateCard
updateCustomerCard
  customerid
  (CardId cardid)
    = updateCard "customers" (getCustomerId customerid) cardid

updateRecipientCard
    :: RecipientId
    -> RecipientCardId
    -> StripeRequest UpdateCard
updateRecipientCard
  recipientid
  (RecipientCardId cardid)
    = updateCard "recipients" (getRecipientId recipientid) cardid

------------------------------------------------------------------------------
-- | Removes a card from a `Customer`
data DeleteCard
type instance StripeReturn DeleteCard = StripeDeleteResult
deleteCard
    :: URL
    -> ID
    -> Text     -- ^ `CardId` associated with `Card` to be deleted
    -> StripeRequest DeleteCard
deleteCard
    prefix
    id_
    cardid_ = request
  where request = mkStripeRequest DELETE url params
        url     = prefix </> id_ </> "cards" </> cardid_
        params  = []

deleteCustomerCard
    :: CustomerId -- ^ `CustomerId` of the `Card` to retrieve
    -> CardId     -- ^ `CardId` associated with `Card` to be deleted
    -> StripeRequest DeleteCard
deleteCustomerCard
    customerid
    (CardId cardid) = deleteCard "customers" (getCustomerId customerid) cardid

deleteRecipientCard
    :: RecipientId     -- ^ `RecipientId` of the `Card` to retrieve
    -> RecipientCardId -- ^ `CardId` associated with `Card` to be deleted
    -> StripeRequest DeleteCard
deleteRecipientCard
    recipientid
    (RecipientCardId cardid)
      = deleteCard "recipients" (getRecipientId recipientid) cardid

------------------------------------------------------------------------------
-- | Retrieve all cards associated with a `Customer`
data GetCards
type instance StripeReturn GetCards = (StripeList Card)
instance StripeHasParam GetCards ExpandParams
instance StripeHasParam GetCards (EndingBefore CardId)
instance StripeHasParam GetCards Limit
instance StripeHasParam GetCards (StartingAfter CardId)
getCards
    :: URL
    -> ID
    -> StripeRequest GetCards
getCards
    prefix
    id_
    = request
  where request = mkStripeRequest GET url params
        url     = prefix </> id_ </> "cards"
        params  = []

getCustomerCards
    :: CustomerId    -- ^ The `CustomerId` associated with the cards
    -> StripeRequest GetCards
getCustomerCards
    customerid
    = getCards "customers" (getCustomerId customerid)

getRecipientCards
    :: RecipientId    -- ^ The `RecipientId` associated with the cards
    -> StripeRequest GetCards
getRecipientCards
    recipientid
    = getCards "recipients" (getRecipientId recipientid)
