{-# LANGUAGE OverloadedStrings #-}
module Web.Stripe.Card
    ( -- * Types
      Card           (..)
    , CardId         (..)
    , CardNumber     (..)
    , ExpMonth       (..)
    , ExpYear        (..)
    , CVC            (..)
    , Name           (..)
    , AddressLine1   (..)
    , AddressLine2   (..)
    , AddressCity    (..)
    , AddressCountry (..)
    , AddressState   (..)
    , AddressZip     (..)
    , RecipientId    (..)
    , RecipientCard  (..)
      -- * API Calls
      -- ** Customers
      -- *** Create Customer Card
    , createCustomerCard
    , createCustomerCardByToken
      -- *** Update Customer Card
    , updateCustomerCard
      -- *** Get Customer Card(s)
    , getCustomerCard
    , getCustomerCards
      -- *** Delete Card
    , deleteCustomerCard
      -- ** Recipients
      -- *** Create Recipient Card
    , createRecipientCard
    , createRecipientCardByToken
      -- *** Get Recipient Card(s)
    , getRecipientCard
    , getRecipientCards
      -- *** Updated Recipient Card
    , updateRecipientCard
      -- *** Delete Recipient Card
    , deleteRecipientCard
    ) where

import           Control.Applicative        ((<$>), (<*>))
import           Data.Aeson                 (FromJSON)
import           Web.Stripe.Client.Internal
import           Web.Stripe.Types

------------------------------------------------------------------------------
-- | Base request function for 'Card' creation
createCardBase
  :: FromJSON a
  => URL
  -> ID
  -> Maybe TokenId
  -> Maybe CardNumber
  -> Maybe ExpMonth
  -> Maybe ExpYear
  -> Maybe CVC
  -> Maybe Name
  -> Maybe AddressCity
  -> Maybe AddressCountry
  -> Maybe AddressLine1
  -> Maybe AddressLine2
  -> Maybe AddressState
  -> Maybe AddressZip
  -> Stripe a
createCardBase
    requestType
    requestId
    tokenId
    cardNumber
    expMonth
    expYear
    cvc
    name
    addressCity
    addressCountry
    addressLine1
    addressLine2
    addressState
    addressZip  = callAPI request
  where request = StripeRequest POST url params
        url     = requestType </> requestId </> "cards"
        params  = getParams [
                     ("card", (\(TokenId x) -> x) <$> tokenId)
                   , ("card[number]", (\(CardNumber x) -> x) <$> cardNumber)
                   , ("card[exp_month]", (\(ExpMonth x) -> toText x) <$> expMonth)
                   , ("card[exp_year]", (\(ExpYear x) -> toText x) <$> expYear)
                   , ("card[cvc]", (\(CVC x) -> x) <$> cvc)
                   , ("name", (\(Name x) -> x) <$> name)
                   , ("address_city", (\(AddressCity x) -> x) <$> addressCity)
                   , ("address_country", (\(AddressCountry x) -> x) <$> addressCountry)
                   , ("address_line1", (\(AddressLine1 x) -> x) <$> addressLine1 )
                   , ("address_line2", (\(AddressLine2 x) -> x) <$> addressLine2 )
                   , ("address_state", (\(AddressState x) -> x) <$> addressState )
                   , ("address_zip", (\(AddressZip x) -> x) <$> addressZip )
                  ]

------------------------------------------------------------------------------
-- | Create a 'Customer' card using a 'Token'
createCustomerCardByToken
    :: CustomerId -- ^ The Customer to which the card will be added
    -> TokenId    -- ^ The Token representative of the card
    -> Stripe Card
createCustomerCardByToken
    (CustomerId customerId)
    tokenId = createCardBase "customer" customerId (Just tokenId)
              Nothing Nothing Nothing
              Nothing Nothing Nothing
              Nothing Nothing Nothing
              Nothing Nothing

------------------------------------------------------------------------------
-- | Create a 'Recipient' card using a 'Token'
createRecipientCardByToken
    :: RecipientId -- ^ The Customer to which the card will be added
    -> TokenId     -- ^ The Token representative of the card
    -> Stripe RecipientCard
createRecipientCardByToken
    (RecipientId recipientId)
    tokenId = createCardBase "customer" recipientId (Just tokenId)
              Nothing Nothing Nothing
              Nothing Nothing Nothing
              Nothing Nothing Nothing
              Nothing Nothing

------------------------------------------------------------------------------
-- | Create a 'Customer' card by 'CardNumber'
createCustomerCard
    :: CustomerId -- ^ 'Customer' to which the card will be added
    -> CardNumber -- ^ 'Card' digits
    -> ExpMonth   -- ^ 'Card' expiration month
    -> ExpYear    -- ^ 'Card' expiration year
    -> CVC        -- ^ 'Card' cvc number
    -> Stripe Card
createCustomerCard
    (CustomerId customerId)
    cardNumber
    expMonth
    expYear
    cvc = createCardBase "customer" customerId Nothing
              (Just cardNumber) (Just expMonth) (Just expYear)
              (Just cvc) Nothing Nothing
              Nothing Nothing Nothing
              Nothing Nothing

------------------------------------------------------------------------------
-- | Create a 'Recipient' 'Card' by 'CardNumber'
createRecipientCard
    :: RecipientId -- ^ 'Recipient' to which the card will be added
    -> CardNumber  -- ^ 'Card' digits
    -> ExpMonth    -- ^ 'Card' expiration month
    -> ExpYear     -- ^ 'Card' expiration year
    -> CVC         -- ^ 'Card' cvc number
    -> Stripe RecipientCard
createRecipientCard
    (RecipientId recipientId)
    cardNumber
    expMonth
    expYear
    cvc = createCardBase "recipient" recipientId Nothing
              (Just cardNumber) (Just expMonth) (Just expYear)
              (Just cvc) Nothing Nothing
              Nothing Nothing Nothing
              Nothing Nothing

------------------------------------------------------------------------------
-- | Update a 'Card', any fields not specified will remain the same
updateCardBase
    :: FromJSON a 
    => URL
    -> ID
    -> CardId
    -> Maybe Name
    -> Maybe AddressCity
    -> Maybe AddressCountry
    -> Maybe AddressLine1
    -> Maybe AddressLine2
    -> Maybe AddressState
    -> Maybe AddressZip
    -> Stripe a
updateCardBase
    requestType
    requestId
    (CardId cardId)
    name
    addressCity
    addressCountry
    addressLine1
    addressLine2
    addressState
    addressZip  = callAPI request
  where request = StripeRequest POST url params
        url     = requestType </> requestId </> "cards" </> cardId
        params  =  getParams [
                     ("name", (\(Name x) -> x) <$> name)
                   , ("address_city", (\(AddressCity x) -> x) <$> addressCity)
                   , ("address_country", (\(AddressCountry x) -> x) <$> addressCountry)
                   , ("address_line1", (\(AddressLine1 x) -> x) <$> addressLine1 )
                   , ("address_line2", (\(AddressLine2 x) -> x) <$> addressLine2 )
                   , ("address_state", (\(AddressState x) -> x) <$> addressState )
                   , ("address_zip", (\(AddressZip x) -> x) <$> addressZip )
                   ]

------------------------------------------------------------------------------
-- | Update a 'Customer' 'Card'
updateCustomerCard
    :: CustomerId
    -> CardId
    -> Maybe Name
    -> Maybe AddressCity
    -> Maybe AddressCountry
    -> Maybe AddressLine1
    -> Maybe AddressLine2
    -> Maybe AddressState
    -> Maybe AddressZip
    -> Stripe Card
updateCustomerCard
    (CustomerId customerId)  = updateCardBase "customer" customerId

------------------------------------------------------------------------------
-- | Update a 'Recipient' 'Card'
updateRecipientCard
    :: RecipientId
    -> CardId
    -> Maybe Name
    -> Maybe AddressCity
    -> Maybe AddressCountry
    -> Maybe AddressLine1
    -> Maybe AddressLine2
    -> Maybe AddressState
    -> Maybe AddressZip
    -> Stripe RecipientCard
updateRecipientCard
    (RecipientId recipientId)  = updateCardBase "recipient" recipientId

------------------------------------------------------------------------------
-- | Base Request for retrieving cards from either a 'Customer' or 'Recipient'
getCardBase
    :: URL        -- ^ The type of the request to support (recipient or customer)
    -> ID         -- ^ 'CustomerId' or 'RecipientId' of the 'Card' to retrieve
    -> CardId     -- ^ 'CardId' of the card to retrieve
    -> Stripe Card
getCardBase
    requestType
    requestId
    (CardId cardId) = callAPI request
  where request = StripeRequest GET url params
        url     = requestType </> requestId </> "cards" </> cardId
        params  = []

------------------------------------------------------------------------------
-- | Get card by 'CustomerId' and 'CardId'
getCustomerCard
    :: CustomerId -- ^ 'CustomerId' of the 'Card' to retrieve
    -> CardId     -- ^ 'CardId' of the card to retrieve
    -> Stripe Card
getCustomerCard
    (CustomerId customerId) = getCardBase "customer" customerId

------------------------------------------------------------------------------
-- | Get card by 'RecipientId' and 'CardId'
getRecipientCard
    :: RecipientId -- ^ 'RecipientId' of the 'Card' to retrieve
    -> CardId      -- ^ 'CardId' of the card to retrieve
    -> Stripe Card
getRecipientCard
       (RecipientId recipientId) = getCardBase "recipient" recipientId

------------------------------------------------------------------------------
-- | Base Request for retrieving 'Customer' or 'Recipient' cards
getCardsBase
    :: FromJSON a
    => URL  -- ^ The type of the request to support ('Recipient' or 'Customer')
    -> ID   -- ^ 'CustomerId' or 'RecipientId' of the 'Card' to retrieve
    -> Stripe (StripeList a)
getCardsBase
    requestType
    requestId = callAPI request
  where request = StripeRequest GET url params
        url     = requestType </> requestId </> "cards"
        params  = []

------------------------------------------------------------------------------
-- | Retrieve all cards associated with a 'Customer'
getCustomerCards
    :: CustomerId   -- ^ The 'CustomerId' associated with the cards
    -> Stripe (StripeList Card)
getCustomerCards
    (CustomerId customerId) = getCardsBase "customer" customerId

------------------------------------------------------------------------------
-- | Retrieve all cards associated with a 'Recipient'
getRecipientCards
    :: RecipientId   -- ^ The 'RecipientId' associated with the cards
    -> Stripe (StripeList RecipientCard)
getRecipientCards
    (RecipientId recipientId) = getCardsBase "recipient" recipientId

------------------------------------------------------------------------------
-- | Base request for 'Card' removal from a 'Customer' or 'Recipient'
deleteCardBase
    :: URL
    -> ID
    -> CardId
    -> Stripe StripeDeleteResult
deleteCardBase
    requestType
    requestId
    (CardId cardId) = callAPI request
  where request = StripeRequest DELETE url params
        url     = requestType </> requestId </> "cards" </> cardId
        params  = []

------------------------------------------------------------------------------
-- | Removes a card from a 'Customer'
deleteCustomerCard
    :: CustomerId
    -> CardId
    -> Stripe StripeDeleteResult
deleteCustomerCard
    (CustomerId customerId) = deleteCardBase "customer" customerId

------------------------------------------------------------------------------
-- | Removes a card from a 'Customer'
deleteRecipientCard
    :: RecipientId
    -> CardId
    -> Stripe StripeDeleteResult
deleteRecipientCard
    (RecipientId recipientId) = deleteCardBase "recipient" recipientId



