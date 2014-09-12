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
      -- * API Calls
      -- * Customers
      -- ** Create Customer Card
    , createCardBase
    , createCard
    , createCardByToken
    -- ** Update Customer Card
    , updateCard
    -- ** Get Customer Card(s)
    , getCard 
    , getCustomerCards
    -- ** Delete Card
    , deleteCard 
      -- * Recipients
      -- ** Create Recipient Card
      -- ** Get Recipient Card(s)
      -- ** Updated Recipient Card
      -- ** Delete Recipient Card
    ) where

import           Control.Applicative        ((<$>), (<*>))
import           Web.Stripe.Client.Internal 
import           Web.Stripe.Types

------------------------------------------------------------------------------
-- | Create card using a Token
createCardByToken 
    :: CustomerId -- ^ The Customer to which the card will be added
    -> TokenId    -- ^ The Token representative of the card
    -> Stripe Card  
createCardByToken 
    customerId
    tokenId = createCardBase customerId (Just tokenId)
              Nothing Nothing Nothing
              Nothing Nothing Nothing
              Nothing Nothing Nothing
              Nothing Nothing 

------------------------------------------------------------------------------
-- | Create card by 'CardNumber'
createCard
    :: CustomerId -- ^ 'Customer' to which the card will be added
    -> CardNumber -- ^ 'Card' digits
    -> ExpMonth   -- ^ 'Card' expiration month
    -> ExpYear    -- ^ 'Card' expiration year
    -> CVC        -- ^ 'Card' cvc number
    -> Stripe Card  
createCard 
    customerId
    cardNumber
    expMonth
    expYear
    cvc = createCardBase customerId Nothing
              (Just cardNumber) (Just expMonth) (Just expYear)
              (Just cvc) Nothing Nothing
              Nothing Nothing Nothing
              Nothing Nothing 

------------------------------------------------------------------------------
-- | Base request type for card creation
createCardBase
  :: CustomerId
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
  -> Stripe Card
createCardBase
    (CustomerId customerId)
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
        url     = "customers" </> customerId </> "cards"
        params  = getParams [
                     ("card",  (\(TokenId x) -> x) <$> tokenId)
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
-- | Update a 'Card', any fields not specified will remain the same
updateCard
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
updateCard
    (CustomerId customerId)
    (CardId cardId)
    name
    addressCity
    addressCountry
    addressLine1
    addressLine2
    addressState
    addressZip  = callAPI request
  where request = StripeRequest POST url params
        url     = "customers" </> customerId </> "cards" </> cardId
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
-- | Get card by 'CustomerId' and 'CardId'
getCard 
    :: CustomerId -- ^ 'CustomerId' of the 'Card' to retrieve
    -> CardId     -- ^ 'CardId' of the card to retrieve
    -> Stripe Card
getCard 
    (CustomerId customerId) 
    (CardId cardId) = callAPI request
  where request = StripeRequest GET url params
        url     = "customers" </> customerId </> "cards" </> cardId
        params  = []

------------------------------------------------------------------------------
-- | Retrieves a 'Customer''s cards
getCustomerCards
    :: CustomerId
    -> Stripe (StripeList Card)
getCustomerCards 
    (CustomerId custId) = callAPI request
  where request = StripeRequest GET url params
        url     = "customers" </> custId </> "cards" 
        params  = []


------------------------------------------------------------------------------
-- | Removes a card from a customer
deleteCard 
    :: CustomerId 
    -> CardId
    -> Stripe StripeDeleteResult
deleteCard 
    (CustomerId custId) 
    (CardId cardId) = callAPI request
  where request = StripeRequest DELETE url params
        url     = "customers" </> custId </> "cards" </> cardId
        params  = []


