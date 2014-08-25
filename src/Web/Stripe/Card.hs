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
    ) where

import           Control.Applicative        ((<$>), (<*>))
import           Data.Monoid
import           Data.Text                  (Text)
import qualified Data.Text.Encoding         as T
import           Web.Stripe.Client.Internal
import           Web.Stripe.Types
import           Web.Stripe.Util

-- | Create card
createCard :: CustomerId -> -- ^ The Customer to which the card will be added
              TokenId ->    -- ^ The Token representative of the card
              Stripe Card   -- ^ The resulting card
createCard (CustomerId cid) (TokenId tokenId) = callAPI request
  where request = StripeRequest POST url params
        url     = "customers" </> cid </> "cards"
        params  = [("card", T.encodeUtf8 tokenId)]

-- | Get card by CustomerID and CardID
getCard :: CustomerId ->  -- ^ CustomerID of the Card to retrieve
           CardId ->      -- ^ ID of the card to retrieve
           Stripe Card
getCard (CustomerId custId) (CardId cardId) = callAPI request
  where request = StripeRequest GET url params
        url     = "customers" </> custId </> "cards" </> cardId
        params  = []

updateCard :: CustomerId ->
              CardId ->
              Maybe AddressCity -> -- addressCity
              Maybe AddressCountry -> -- addressCountry
              Maybe AddressLine1 -> -- addressLine1
              Maybe AddressLine2 -> -- addressLine2
              Maybe AddressState -> -- addressState
              Maybe AddressZip -> -- addressZip
              Maybe ExpMonth -> -- expMonth
              Maybe ExpYear -> -- expYear
              Maybe Name -> -- name
              Stripe Card
updateCard (CustomerId custId)
           (CardId cardId)
           addressCity
           addressCountry
           addressLine1
           addressLine2
           addressState
           addressZip
           expMonth
           expYear
           name = callAPI request
  where request = StripeRequest POST url params
        url     = "customers" </> custId </> "cards" </> cardId
        params  = [ (k, v) | (k, Just v) <- [
                     ("address_city",  (\(AddressCity x) -> T.encodeUtf8 x) <$> addressCity)
                   , ("address_country", (\(AddressCountry x) -> T.encodeUtf8 x) <$> addressCountry)
                   , ("address_line1", (\(AddressLine1 x) -> T.encodeUtf8 x) <$> addressLine1 )
                   , ("address_line2", (\(AddressLine2 x) -> T.encodeUtf8 x) <$> addressLine2 )
                   , ("address_state", (\(AddressState x) -> T.encodeUtf8 x) <$> addressState )
                   , ("address_zip", (\(AddressZip x) -> T.encodeUtf8 x) <$> addressZip )
                   , ("exp_month", (\(ExpMonth x) -> toBS x) <$> expMonth )
                   , ("exp_year", (\(ExpYear x) -> toBS x) <$> expYear )
                   , ("name", (\(Name x) -> T.encodeUtf8 x) <$> name )
                   ]
                 ]

deleteCard :: CustomerId -> CardId -> Stripe StripeDeleteResult
deleteCard (CustomerId custId) (CardId cardId) = callAPI request
  where request = StripeRequest DELETE url params
        url     = "customers" </> custId </> "cards" </> cardId
        params  = []
