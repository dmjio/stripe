{-# LANGUAGE OverloadedStrings #-}
module Web.Stripe.Card
    ( -- * Types
      Card(..)
    , CardId(..)
      -- * API Functions
    , createCard 
    , getCard 
    , updateCard 
    , deleteCard 
    , getCards
    ) where

import           Control.Applicative        ((<$>), (<*>))
import           Web.Stripe.Client.Internal
import           Web.Stripe.Types

-- | Create card
createCardByToken 
    :: CustomerId -- ^ The Customer to which the card will be added
    -> TokenId    -- ^ The Token representative of the card
    -> Stripe Card  
createCardByToken 
    (CustomerId cid)
    (TokenId tokenId) = callAPI request
  where request = StripeRequest POST url params
        url     = "customers" </> cid </> "cards"
        params  = getParams [
                   ("card", Just tokenId)
                  ]


-- | Get card by CustomerID and CardID
getCard 
    :: CustomerId -- ^ CustomerID of the Card to retrieve
    -> CardId     -- ^ ID of the card to retrieve
    Stripe Card
getCard 
    (CustomerId custId) 
    (CardId cardId) = callAPI request
  where request = StripeRequest GET url params
        url     = "customers" </> custId </> "cards" </> cardId
        params  = []

createCard
  :: CustomerId
  -> ExpMonth
  -> ExpYear
  -> CVC
  -> Maybe Name
  -> Maybe AddressCity
  -> Maybe AddressCountry
  -> Maybe AddressLine1
  -> Maybe AddressLine2
  -> Maybe AddressState
  -> Maybe AddressZip
  -> Stripe Card
createCard customerId expMonth expYear cvc
    = modifyCard customerId Nothing (Just expMonth) (Just expYear) (Just cvc)

updateCard
    :: CustomerId
    -> CardId
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
updateCard customerId cardId = 
    modifyCard customerId (Just cardId)

modifyCard 
     :: CustomerId
     -> Maybe CardId
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
modifyCard 
    (CustomerId custId)
    cardId
    expMonth
    expYear
    cvc
    name
    addressCity
    addressCountry
    addressLine1
    addressLine2
    addressState
    addressZip = callAPI request
  where request = StripeRequest POST url params
        url     = let requestUrl = "customers" </> custId </> "cards"
                  in maybe requestUrl (\(CardId c) -> requestUrl </> c) cardId
        params  = getParams [
                     ("address_city",  (\(AddressCity x) -> x) <$> addressCity)
                   , ("address_country", (\(AddressCountry x) -> x) <$> addressCountry)
                   , ("address_line1", (\(AddressLine1 x) -> x) <$> addressLine1 )
                   , ("address_line2", (\(AddressLine2 x) -> x) <$> addressLine2 )
                   , ("address_state", (\(AddressState x) -> x) <$> addressState )
                   , ("address_zip", (\(AddressZip x) -> x) <$> addressZip )
                   , ("cvc", (\(ExpMonth x) -> toText x) <$> expMonth )
                   , ("exp_month", (\(ExpMonth x) -> toText x) <$> expMonth )
                   , ("exp_year", (\(ExpYear x) -> toText x) <$> expYear )
                   , ("name", (\(Name x) -> x) <$> name )
                  ]

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

getCards 
    :: CustomerId
    -> Stripe (StripeList Card)
getCards 
    (CustomerId custId) = callAPI request
  where request = StripeRequest GET url params
        url     = "customers" </> custId </> "cards"
        params  = []

